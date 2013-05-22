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
    to_html/2,
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

-define(COMET_FLUSH_EMPTY, 55000).
-define(COMET_PIGGY_FLUSH, 50).

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

%%
allowed_methods(ReqData, Context) ->
    {['GET', 'POST'], ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{"text/html", to_html}], ReqData, Context}.

%% @doc Handle a get request, this is the subframe.
%%
to_html(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Context2 = z_context:ensure_all(Context1),
    Context3 = z_context:set_noindex_header(Context2),
    Template = z_context:get(template, Context3, "bus.tpl"),
    Rendered = z_template:render(Template, z_context:get_all(Context3), Context3),
    {Output, OutputContext} = z_context:output(Rendered, Context3),
    ?WM_REPLY(Output, OutputContext).


%% @doc Wait for a message to send to the client.
%%
process_post(ReqData, Context) ->
    %% Process post
    Context1 = ?WM_REQ(ReqData, Context),
    Context2 = z_context:set_reqdata(ReqData, Context1),
    case wrq:get_qs_value("m", ReqData) of
        undefined -> 
            process_send(Context2);
        _ ->
            BusPid = get_bus_handler(Context),
            process_push(BusPid, Context2)
    end.

%%
%% Comet handler.
%% 

%% @doc Receive a message from the client and send it to the bus handler.
%% 
%% TODO: Temporarily attach this process as comet handler, 
%% then we can get a quick response back via this socket and leave 
%%
process_send(Context) ->
    ReqData = z_context:get_reqdata(Context),
    case wrq:req_body(ReqData) of
        {undefined, Rd} ->
            %% Nothing, just ignore it.
            Context1 = z_context:set_reqdata(Rd, Context),
            ?WM_REPLY(true, Context1);
        {Msg, Rd} ->
            %% Yes, we have a message, send it to the bus handler.
            BusPid = get_bus_handler(Context),
            bus_handler:bus_message(BusPid, Msg),

            %% poll process intact
            % ?DEBUG(attach_post),
            case bus_handler:attach_post(self(), BusPid) of
                connected ->
                    receive
                        {send_data, Data} ->
                            bus_handler:detach_post(self(), BusPid),
                            RD = z_context:get_reqdata(Context),
                            RD1 = wrq:append_to_response_body(Data, RD),
                            Context1 = z_context:set_reqdata(RD1, Context),
                            ?WM_REPLY(true, Context1)
                    after 
                        ?COMET_PIGGY_FLUSH ->
                            bus_handler:detach_post(self(), BusPid),
                            Context1 = z_context:set_reqdata(Rd, Context),
                            ?WM_REPLY(true, Context1)
                    end;
                _ ->
                    Context1 = z_context:set_reqdata(Rd, Context),
                    ?WM_REPLY(true, Context1)
            end
    end.

%% @doc Wait for all scripts to be pushed to the user agent.
%%
process_push(BusPid, Context) ->
    bus_handler:attach_comet(self(), BusPid),
    Context1 = receive
        {send_queued_data, Messages} ->
            RD = z_context:get_reqdata(Context),
            RD1 = wrq:append_to_response_body(Messages, RD),
            z_context:set_reqdata(RD1, Context);           
        {send_data, Data} ->
            RD = z_context:get_reqdata(Context),
            RD1 = wrq:append_to_response_body(Data, RD),
            z_context:set_reqdata(RD1, Context)
        after 
            ?COMET_FLUSH_EMPTY ->
                Context
    end,
    bus_handler:detach_comet(self(), BusPid),
    ?WM_REPLY(true, Context1).


%%
%% Bus.
%%

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
                    {ok, Pid} = z_pool:run(bus_pool,
                        [Name, Handler, Context], Context),
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

%%
%% Websocket stuff.
%%

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

websocket_terminate(_Reason, Context) ->
    Pid = get_bus_handler(Context),
    bus_handler:detach_websocket(self(), Pid).

