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

%% interface functions
-export([attach_websocket/2]).

-record(state, {
    handler, 
    websocket_pid=undefined, 
    comet_pid=undefined, 
    context
}).

attach_websocket(WsPid, Pid) ->
    ?DEBUG(attach_websocket),
    gen_server:cast(Pid, {attach_websocket, WsPid}).

%% 
start_link(Name, HandlerModule, Context) ->
    {ok, Pid} = gen_server:start_link(?MODULE, [Name, HandlerModule, Context], []),
    {ok, Pid}.

%% @doc Send a message to the bus.
bus_message(Pid, Msg) ->
    gen_server:cast(Pid, {bus_message, Msg}).

bus_info(Pid, Info) ->
    gen_server:cast(Pid, {bus_info, Info}).

%% @doc Initiates the server.
init([Name, HandlerModule, Context]) ->
    process_flag(trap_exit, true),
    z_proc:register(Name, self(), Context),
    {ok, Context1} = HandlerModule:bus_init(Name, Context), 
    {ok, #state{handler=HandlerModule, context=Context1}}.

%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.

%% @doc Handle the next step in the module initialization.
handle_cast({attach_websocket, WsPid}, #state{websocket_pid=undefined, comet_pid=undefined}=State) ->
    case z_utils:is_process_alive(WsPid) of
        true ->
            erlang:monitor(process, WsPid),
            StateWs = State#state{websocket_pid=WsPid},
            {noreply, StateWs};
        false ->
            {noreply, State}
    end;
    
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

handle_info({send_data, Data}, #state{websocket_pid=WsPid}=State) when is_pid(WsPid) ->
    WsPid ! {send_data, Data},
    {noreply, State};
handle_info({send_data, _Data}, #state{comet_pid=CometPid}=State) when is_pid(CometPid) ->
    ?DEBUG({'TODO'}),
    {noreply, State};
handle_info({send_data, _Data}, #state{websocket_pid=undefined, comet_pid=undefined}=State) ->
    ?DEBUG({'stopping, no socket attached'}),
    {stop, {error, no_connection}, State};
handle_info({'DOWN', _MonitorRef, process, WsPid, _Info}, #state{websocket_pid=WsPid}=State) ->
    {stop, normal, State#state{websocket_pid=undefined}};
handle_info({'DOWN', _MonitorRef, process, CometPid, _Info}, #state{comet_pid=CometPid}=State) ->
    {stop, normal, State#state{comet_pid=undefined}};

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

