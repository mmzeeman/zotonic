%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2013 Maas-Maarten Zeeman
%% @doc Handles engine io long polls from the user agent

%% Copyright 2013 Marc Worrell
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

-module(controller_engine_io).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-export([
    init/1,
    service_available/2, 
    malformed_request/2,
    options/2,
    upgrades_provided/2,
    allowed_methods/2,
    content_types_provided/2,
    process_post/2,
    process_get/2
    ]).

% websocket handler exports.
-export([
    websocket_start/2,
    websocket_init/1,
    websocket_message/2,
    websocket_info/2,
    websocket_terminate/2]).

-include_lib("controller_webmachine_helper.hrl").
-include_lib("include/zotonic.hrl").


%% Timeout for comet flush when there is no data, webmachine 0.x had a timeout of 60 seconds, so leave after 55
-define(COMET_FLUSH_EMPTY, 55000).

%% Timeout for comet flush when there is data, allow for 50 msec more to gather extra data before flushing
-define(COMET_FLUSH_DATA,  50).


init(ConfigProps) -> 
    {ok, ConfigProps}.

%%
service_available(ReqData, ConfigProps) ->
    ?DEBUG(service_available),
    NewContext = z_context:new(ReqData, ?MODULE),
    ConfigContext = z_context:set(ConfigProps, NewContext),
    NoindexContext = z_context:set_noindex_header(ConfigContext),
    QsContext = z_context:ensure_qs(NoindexContext),

    case protocol(QsContext) of
        flashsocket -> %% No support for flash sockets.
            ?WM_REPLY(false, QsContext);
        Protocol -> 
            ProtocolContext = z_context:set(protocol, Protocol, QsContext),
            ?WM_REPLY(true, ProtocolContext)
    end.

%%
malformed_request(ReqData, Context) ->
    ?DEBUG(malformed),
    Context1 = ?WM_REQ(ReqData, Context),
    case z_context:get(protocol, Context1) of
        undefined -> 
            {true, ReqData, Context1};
        _ ->
            AllContext = z_context:ensure_all(Context1),
            {false, ReqData, AllContext}
    end.

%%
%% Needed for pre-flight cors checks.
%%
options(ReqData, _Context) ->
    ?DEBUG(pre_flight_cors),
    [].

%% @doc Possible connection upgrade
upgrades_provided(ReqData, Context) ->
    upgrades_provided(protocol(Context), ReqData, Context).


%%
%% Provide the websocket upgrade for when the websocket when the websocket 
%% protocol is used.
upgrades_provided(websocket, ReqData, Context) ->
    {[{"WebSocket", websocket_start}], ReqData, Context};
upgrades_provided(_, ReqData, Context) ->
    {[], ReqData, Context}.

allowed_methods(ReqData, Context) ->
    {['GET', 'POST'], ReqData, Context}.

content_types_provided(ReqData, Context) -> 
    {[{"text/plain", process_get}], ReqData, Context}.

%% 
process_get(ReqData, #context{page_pid=PagePid}=Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    PageId = z_session_page:page_id(Context), 
    
    %% What happens now depends on the state... 
    erlang:monitor(process, PagePid),
    TimerRef = erlang:send_after(55000, self(), flush),
    process_get_loop(Context1, PageId, TimerRef, false).


%%
open_msg(PageId, Context) ->
    Msg = [{sid, PageId}, 
           {upgrades, []}, 
           {pingInterval, 15000}, 
           {pingTimeout, 30000}],
    mochijson:binary_encode(z_json:to_mochijson(Msg, Context)).

%% TODO: Note, the message can be utf-8. In that case the length should be 
%% the number of unicode characters and not the number of bytes...
xhr_encode(Msg) ->
    Size = erlang:iolist_size(Msg),
    [integer_to_list(Size), $:, Msg].


% %%
% process_get(xhr, ReqData, Context) ->
%     %% poll
%     %%
%     Context1 = ?WM_REQ(ReqData, Context),
%     Context2 = z_context:ensure_all(Context1),
%     Context3 = z_context:set_noindex_header(Context2),
%     Rendered = "0{sid:'1234',upgrades:[],pingTimeout:60}",
%     ?DEBUG(xhr_get),
%     {Output, OutputContext} = z_context:output(Rendered, Context3),
%     ?WM_REPLY(Output, OutputContext);


process_get_loop(#context{page_pid=PagePid}=Context, PageId, TimerRef, HasData) ->
    receive
        flush ->
            ?DEBUG(flush),
            erlang:cancel_timer(TimerRef),

            OpenMsg = [$0, ?DEBUG(open_msg(PageId, Context))],
            {Output, OutputContext} = z_context:output(xhr_encode(OpenMsg), Context),
            ?WM_REPLY(Output, OutputContext);
        {'DOWN', _MonitorRef, process, PagePid, _Info} ->
            ?DEBUG(page_down),
            self() ! flush,
            process_get_loop(Context, PageId, TimerRef, false)
    end.


%% @doc Return the engine.io protocol
%% 
protocol(Context) ->
    case z_context:get_q(transport, Context) of
        "polling" -> 
            case z_context:get_q(j, Context) of
                undefined -> xhr;
                ResponseIndex -> {jsonp, ResponseIndex}
            end;
        "websocket" -> websocket;
        "flashsocket" -> flashsocket;
        _ -> undefined
    end.

%% 
%% Do something with the received data. 
%% Note: It is handy to delay responding for a little while. Some messages
%% cause immediate response messages which can be returned immediately in 
%% this request. I'm thinking about a 20/30 ms delay. That way long-polling
%% connecion can be left in place.
process_post(ReqData, Context) ->
    ?DEBUG(post),
    Context1 = ?WM_REQ(ReqData, Context),
    ?WM_REPLY(true, Context1).

%% @doc Initiate the websocket connection upgrade
websocket_start(ReqData, Context) ->
    Context1 = z_context:set(ws_handler, ?MODULE, Context),
    controller_websocket:websocket_start(ReqData, Context1).

%% ------ built in websocket handler -----

%% ws handler calls.
websocket_init(_Context) ->
    ?DEBUG(ws_init).

%% ws handler
websocket_message(Msg, _Context) ->
    ?DEBUG({ws_msg, Msg}).

websocket_info(Msg, _Context) -> 
    ?DEBUG({ws_info, Msg}).

websocket_terminate(Reason, _Context) ->
    ?DEBUG({ws_terminate, Reason}).