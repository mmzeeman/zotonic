%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2013 Maas-Maarten Zeeman

%% Copyright 2013 Maas-Maarten Zeeman
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(bus_handler).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>>").

-include_lib("zotonic.hrl").

-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/3]).

-export([bus_message/2, bus_info/2]).

-export([send_queued_ws_messages/2]).

%% interface functions
-export([
    attach_websocket/2,
    detach_websocket/2,
    attach_post/2,
    detach_post/2,
    attach_comet/2, 
    detach_comet/2
]).

-record(state, {
    handler, 
    
    websocket_pid=undefined, 
    comet_pid=undefined,
    post_pid=undefined,

    monitor_ref=undefined,
    monitor_post_ref=undefined,
    
    messages=queue:new(),
    last_detach=undefined,
    context
}).

-define(INTERVAL_MSEC, (?SESSION_PAGE_TIMEOUT div 2) * 1000).

% @doc Attach the websocket process to the bus handler.
%
attach_websocket(WsPid, BusPid) ->
    gen_server:cast(BusPid, {attach_websocket, WsPid}).

% @doc Detach the websocket from the bus handler
%
detach_websocket(WsPid, BusPid) ->
    gen_server:cast(BusPid, {detach_websocket, WsPid}).

% @doc Attach the comet push process to the bus handler.
%
attach_comet(CometPid, BusPid) ->
    gen_server:cast(BusPid, {attach_comet, CometPid}).

% @doc Detach the comet process from the bus handler.
%
detach_comet(CometPid, BusPid) ->
    gen_server:cast(BusPid, {detach_comet, CometPid}).

% @doc Attach the message push process
attach_post(PostPid, BusPid) ->
    gen_server:call(BusPid, {attach_post, PostPid}).

% @doc Attach the message push process
detach_post(PostPid, BusPid) ->
    gen_server:cast(BusPid, {detach_post, PostPid}).

%% 
start_link(Name, HandlerModule, Context) ->
    {ok, _Pid} = gen_server:start_link(?MODULE, [Name, HandlerModule, Context], []).

%% @doc Send a message to the bus.
bus_message(Pid, Msg) ->
    gen_server:cast(Pid, {bus_message, Msg}).

bus_info(Pid, Info) ->
    gen_server:cast(Pid, {bus_info, Info}).

%% gen-server callbacks.

%% @doc Initiates the server.
init([Name, HandlerModule, Context]) ->
    process_flag(trap_exit, true),
    trigger_check_timeout(),
    z_proc:register(Name, self(), Context),
    {ok, Context1} = HandlerModule:bus_init(Name, Context), 
    {ok, #state{handler=HandlerModule, context=Context1, last_detach=z_utils:now()}}.

%% @doc Trap unknown calls
handle_call({attach_post, PostPid}, _From, #state{post_pid=undefined}=State) ->
    case z_utils:is_process_alive(PostPid) of
        true ->
            Ref = erlang:monitor(process, PostPid),
            {reply, connected, State#state{post_pid=PostPid, monitor_post_ref=Ref}};
        false ->
            {reply, not_alive, State}
    end;
handle_call({attach_post, _PostPid}, _From, State) ->
    {reply, already_connected, State};
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.

%% @doc Handle the next step in the module initialization.
handle_cast({attach_websocket, WsPid}, #state{websocket_pid=undefined, 
        comet_pid=undefined}=State) ->
    case z_utils:is_process_alive(WsPid) of
        true ->
            Ref = erlang:monitor(process, WsPid),
            StateWs = State#state{websocket_pid=WsPid, monitor_ref=Ref},
            StateMsg = handle_queued_ws_messages(StateWs), 
            {noreply, StateMsg};
        false ->
            {noreply, State}
    end;
handle_cast({attach_websocket, WsPid}, State) ->
    ?DEBUG({attach_websocket, WsPid, already_attached, State}),
    {noreply, State};

handle_cast({detach_websocket, WsPid}, #state{websocket_pid=WsPid, monitor_ref=Ref}=State) ->
    erlang:demonitor(Ref, [flush]),
    {noreply, State#state{websocket_pid=undefined, monitor_ref=undefined, last_detach=z_utils:now()}};  

handle_cast({detach_websocket, _WsPid}, #state{websocket_pid=undefined}=State) ->
    ?DEBUG(websocket_not_attached),
    {noreply, State};  

%% @doc Attach a comet pid. 
handle_cast({attach_comet, CometPid}, #state{comet_pid=undefined, websocket_pid=undefined}=State) ->
    case z_utils:is_process_alive(CometPid) of
        true ->
            Ref = erlang:monitor(process, CometPid),
            StateComet = State#state{comet_pid=CometPid, monitor_ref=Ref},
            StateMsg = handle_queued_comet_messages(StateComet), 
            {noreply, StateMsg};
        false ->
            {noreply, State}
    end;
handle_cast({attach_comet, CometPid}, #state{comet_pid=_CPid, websocket_pid=undefined, monitor_ref=Ref}=State) ->
    case z_utils:is_process_alive(CometPid) of
        true ->
            erlang:demonitor(Ref, [flush]),
            NewRef = erlang:monitor(process, CometPid),
            StateComet = State#state{comet_pid=CometPid, monitor_ref=NewRef},
            StateMsg = handle_queued_comet_messages(StateComet), 
            {noreply, StateMsg};
        false ->
            {noreply, State}
    end;
handle_cast({attach_comet, CometPid}, #state{comet_pid=undefined, websocket_pid=_WPid, monitor_ref=Ref}=State) ->
    case z_utils:is_process_alive(CometPid) of
        true ->
            erlang:demonitor(Ref, [flush]),
            NewRef = erlang:monitor(process, CometPid),
            StateComet = State#state{comet_pid=CometPid, websocket_pid=undefined, monitor_ref=NewRef},
            StateMsg = handle_queued_comet_messages(StateComet), 
            {noreply, StateMsg};
        false ->
            {noreply, State}
    end;
handle_cast({detach_comet, CometPid}, #state{comet_pid=CometPid, monitor_ref=Ref}=State) ->
    erlang:demonitor(Ref, [flush]),
    {noreply, State#state{comet_pid=undefined, monitor_ref=undefined, last_detach=z_utils:now()}};
handle_cast({detach_comet, _CometPid}, State) ->
    %% ignore
    {noreply, State};

%% @doc Attach a post pid. It will get prio over everything else.
%%
handle_cast({detach_post, PostPid}, #state{post_pid=PostPid, monitor_post_ref=Ref}=State) ->
    erlang:demonitor(Ref, [flush]),
    {noreply, State#state{post_pid=undefined, monitor_post_ref=Ref, last_detach=z_utils:now()}};

%%
%%
%%
    
handle_cast({bus_message, Msg}, #state{handler=Handler, context=Context}=State) ->
    Handler:bus_message(Msg, Context),
    {noreply, State};

handle_cast({bus_info, Msg}, #state{handler=Handler, context=Context}=State) ->
    Handler:bus_info(Msg, Context),
    {noreply, State};

%% @doc Trap unknown casts
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.

%% @doc Handling all non call/cast messages

% @doc Send data to websocket
%
handle_info({send_data, Data}, #state{post_pid=PostPid}=State) when is_pid(PostPid) ->
    PostPid ! {send_data, Data},
    {noreply, State};

handle_info({send_data, Data}, #state{websocket_pid=WsPid}=State) when is_pid(WsPid) ->
    controller_websocket:websocket_send_data(WsPid, Data),
    {noreply, State};

% @doc Send data to comet
%
handle_info({send_data, Data}, #state{comet_pid=CometPid}=State) when is_pid(CometPid) ->
    CometPid ! {send_data, Data},
    {noreply, State};

% @doc No comet or websocket attached yet. Queue the data.
%
handle_info({send_data, Data}, #state{websocket_pid=undefined, 
        comet_pid=undefined, messages=Msg}=State) ->
    Msg1 = queue:in(Data, Msg),
    {noreply, State#state{messages=Msg1}};

%% @doc Do not timeout while there is a comet or websocket process attached
handle_info(check_timeout, #state{websocket_pid=WsPid, comet_pid=CometPid}=State) when is_pid(CometPid) or is_pid(WsPid)->
    z_utils:flush_message(check_timeout),
    trigger_check_timeout(),
    {noreply, State};

%% @doc Give the comet process some time to come back, timeout afterwards
handle_info(check_timeout, State) ->
    z_utils:flush_message(check_timeout),
    Timeout = State#state.last_detach + ?SESSION_PAGE_TIMEOUT,
    case Timeout =< z_utils:now() of
        true -> 
            {stop, normal, State};
        false ->
            trigger_check_timeout(),
            {noreply, State}
    end;

%%
handle_info({'DOWN', _MonitorRef, process, PostPid, _Info}, #state{post_pid=PostPid}=State) ->
    {stop, normal, State#state{post_pid=undefined, monitor_post_ref=undefined}};
handle_info({'DOWN', _MonitorRef, process, WsPid, _Info}, #state{websocket_pid=WsPid}=State) ->
    {stop, normal, State#state{websocket_pid=undefined, monitor_ref=undefined}};
handle_info({'DOWN', _MonitorRef, process, CometPid, _Info}, #state{comet_pid=CometPid}=State) ->
    {stop, normal, State#state{comet_pid=undefined, monitor_ref=undefined}};

%%
handle_info(Info, #state{handler=Handler, context=Context}=State) ->
    Handler:bus_info(Info, Context),
    {noreply, State}.

%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, _State) ->
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================

%% @doc Trigger sending a check_timeout message.
trigger_check_timeout() ->
    erlang:send_after(?INTERVAL_MSEC, self(), check_timeout).


%% -- 

handle_queued_ws_messages(#state{messages=Msgs}=State) ->
    case queue:len(Msgs) of
        N when N =< 0 ->
            State; 
        _ ->
            spawn_link(?MODULE, send_queued_ws_messages, [self(), Msgs]),
            State#state{messages=queue:new()}
    end.
            
send_queued_ws_messages(Pid, Msgs) ->
    case queue:out(Msgs) of 
        {empty, _} ->
            done;
        {{value, Data}, Msgs1} ->
            ?DEBUG({send_queued, queue:to_list(Msgs)}),
            Pid ! {send_data, Data},
            send_queued_ws_messages(Pid, Msgs1)
    end.

%% --

handle_queued_comet_messages(#state{comet_pid=CometPid, messages=Msgs}=State) ->
    case queue:to_list(Msgs) of
        [] ->
            State;
        Messages ->  
            CometPid ! {send_queued_data, Messages},
            State#state{messages=queue:new()}
    end.
