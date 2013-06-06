%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2013 Maas-Maarten Zeeman
%% @doc Bus provides a generic client/server message bus.

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

-module(mod_bus).

-mod_title("Client/Server Message Bus").
-mod_description("Provides persistent client/server message bus.").
-mod_prio(9999).
-mod_depends([base]).
-mod_provides([bus]).

-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-include_lib("zotonic.hrl").

-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

-record(state, {
    context
    }).

%%====================================================================
%% API
%%====================================================================
%% @spec start_link(Args) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link(Args) when is_list(Args) ->
    {context, Context} = proplists:lookup(context, Args),
    {ok, _Pid} = gen_server:start_link({local, name(Context)}, ?MODULE, Args, []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init(Args) ->
    process_flag(trap_exit, true),
    {context, Context} = proplists:lookup(context, Args),
    Context1 = z_context:new(Context),
    start_pool(Context1),
    {ok, #state{context=Context1}}.

%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.

%% @doc Trap unknown casts
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.

%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handling all non call/cast messages
handle_info(Info, State) ->
    ?DEBUG({unknown_info, Info}),
    {noreply, State}.

%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, State) ->
    stop_pool(State#state.context).

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================

% @doc Start the bus pool.
start_pool(Context) ->
    ?DEBUG(start_bus_pool),
    z_pool:start(bus_pool, {bus_handler, start_link, []}, 1000, Context).
    
% @doc Stop the bus pool and all the bus handlers in it.
stop_pool(Context) ->
    ?DEBUG(stop_bus_pool),
    z_pool:stop(bus_pool, Context),
    ok.
    
% @doc
name(#context{host=Host}) -> 
    name(Host);
name(Host) -> 
    z_utils:name_for_host(?MODULE, Host).

