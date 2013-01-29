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

-module(controller_bus).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-export([
    init/1, 
    forbidden/2,
    allowed_methods/2,
    upgrades_provided/2,
    content_types_provided/2,
    process_post/2]).

% websocket handler exports.
-export([
    websocket_start/2,
    websocket_init/1,
    websocket_message/2,
    websocket_info/2,
    websocket_terminate/2]).

-include_lib("controller_webmachine_helper.hrl").
-include_lib("zotonic.hrl").

init(DispatchArgs) ->
	{ok, DispatchArgs}.

%% @doc The request must have a valid session cookie.
%%
%% TODO: add comet transport.
forbidden(ReqData, DispatchArgs) ->
	Context = z_context:new(ReqData),
    Context1 = z_context:continue_session(Context),
    case z_context:has_session(Context1) of
    	true ->
    	    Context2 = z_session:ensure_page_session(Context1),
            Context3 = z_context:set(DispatchArgs, Context2),
            ensure_bus(Context3),
    	    ?WM_REPLY(false, Context3);
    	_ ->
            ?WM_REPLY(true, Context1)
    end.

%% @doc Possible connection upgrades
upgrades_provided(ReqData, Context) ->
    {[{"WebSocket", websocket_start}], ReqData, Context}.

allowed_methods(ReqData, Context) ->
    {['GET', 'POST'], ReqData, Context}.

content_types_provided(ReqData, Context) ->
    %% When handling a POST the content type function is not used, so supply false for the function.
    { [{"application/x-javascript", false}], ReqData, Context }.

%% @doc wait 
process_post(ReqData, Context) ->
	Context1 = ?WM_REQ(ReqData, Context),
	Context1.


%% @doc Ensure that the stream process is started, and get
%% a handler.
ensure_bus(Context) ->
    Name = bus_name(Context),
    case z_proc:whereis(Name, Context) of
        undefined -> 
            case z_context:get(bus_handler, Context) of
                undefined ->
                    error({error, no_bus_handler});
                Handler -> 
                    {ok, Pid} = z_pool:run(bus_pool, [Name, Handler, Context], Context),
                    Pid
            end;
        Pid -> Pid
    end.

% @doc Get the name of the bus_handler. This is needed to retrieve
% it from the pool of handlers.
bus_name(Context) ->
    Dispatch = z_context:get(zotonic_dispatch, Context),
    PageId = Context#context.page_id,
    {stream, Dispatch, PageId}.

% @doc return the pid of the bus_handler.
get_bus_handler(Context) ->
    BusName = bus_name(Context),
    case z_proc:whereis(BusName, Context) of
        undefined -> error({error, no_bus_handler});
        Pid -> Pid
    end.

%% @doc Initiate the websocket connection upgrade
websocket_start(ReqData, Context) ->
    Context1 = z_context:set(ws_handler, ?MODULE, Context),
    controller_websocket:websocket_start(ReqData, Context1).


%% ------ built in websocket handler -----

%% ws handler calls.
websocket_init(Context) ->
    Pid = get_bus_handler(Context),
    bus_handler:attach_websocket(self(), Pid).

%% ws handler 
websocket_message(Msg, Context) ->
    Pid = get_bus_handler(Context),
    bus_handler:bus_message(Pid, Msg).

websocket_info(Msg, Context) ->
    Pid = get_bus_handler(Context),
    bus_handler:bus_info(Pid, Msg).

websocket_terminate(_Reason, _Context) ->
    ok.
