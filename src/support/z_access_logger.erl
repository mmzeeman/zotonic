%% @author Maas-Maarten Zeeman <mmzeema@xs4all.nl>
%% @copyright 2014 Maas-Maarten Zeeman
%% Date: 2014-06-08
%% @doc Takes care of access logging.

%% Copyright 2014 Maas-Maarten Zeeman
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

-module(z_access_logger).

%% Logging interface
-export([start/0, log/1]).

-export([host/1, user_id/1, referrer/1, user_agent/1]).

%% Common log formats.
-export([
    combined/1, 
    combined_vhost/1 
]).

-include_lib("zotonic.hrl").
-include_lib("webzmachine/include/webmachine_logger.hrl").

%% Called when access logging needs to start.
start() ->
    LogDir = z_config:get_dirty(log_dir),
    application:set_env(webzmachine, log_dir, LogDir),
    webmachine_sup:start_logger(webmachine_logger),

    case z_config:get_dirty(enable_perf_logger) of
        true ->
            application:set_env(webzmachine, enable_perf_logger, true),
            webmachine_sup:start_perf_logger(LogDir);
        _ ->
            ignore
    end.


%% Called to log a message.
log(LogData) ->
    webmachine_logger:log_message(combined_vhost(LogData)).

%%
%% Supported log formats.
%%

combined(LogData) ->
    [ip(LogData), 32, $-, 32, user(LogData), 32, $[, format_date(finish_time(LogData)), $], 32,
        $", webmachine_util:fmt_method(method(LogData)), 32, path(LogData), 32, format_version(version(LogData)), $", 32,
        integer_to_list(response_code(LogData)), 32, response_length(LogData), 32,
        $", referrer(LogData), $", 32,
        $", user_agent(LogData), $"
    ].

combined_vhost(LogData) ->
    [ip(LogData), 32, $-, 32, user(LogData), 32, $[, format_date(finish_time(LogData)), $], 32,
        $", webmachine_util:fmt_method(method(LogData)), 32, path(LogData), 32, format_version(version(LogData)), $", 32,
        integer_to_list(response_code(LogData)), 32, response_length(LogData), 32,
        $", referrer(LogData), $", 32,
        $", user_agent(LogData), $", 32,
        vhost(LogData)
    ].


%%
%% Helpers
%%

% @doc Get a nicely formatted ip address.
ip(LogData) ->
    format_ip(peer(LogData)).

% @doc Get a nicely formatted user id.
user(LogData) ->
    format_user_id(user_id(LogData)).

% @doc Get a nicely vhost identifier.
vhost(LogData) ->
    atom_to_list(host(LogData)).

% @doc Get the zotonic_host.
host(#wm_log_data{}=LogData) ->
    webmachine_logger:get_metadata(zotonic_host, LogData).

% @doc Get the user_id 
user_id(#wm_log_data{}=LogData) ->
    webmachine_logger:get_metadata(zotonic_user_id, LogData).

% @doc Format the user id.
% TODO: return a user name instead of a number
format_user_id(undefined) ->
    $-;
format_user_id(Number) ->
    integer_to_list(Number).

% @doc Return the http version
format_version({Major, Minor}) ->
    ["HTTP/", integer_to_list(Major), $., integer_to_list(Minor)].

% @doc Return a formatted ip address.
format_ip(IP) when is_tuple(IP) ->
    inet_parse:ntoa(IP);
format_ip(undefined) ->
    "0.0.0.0";
format_ip(HostName) ->
    HostName.

% @doc Return the date format found in apache logs.
format_date({_,_,_}=TimeStamp) ->
    format_date(calendar:now_to_datetime(TimeStamp));
format_date(DateTime) ->
    dh_date:format("d/F/Y:H:m:s +0000", DateTime).


% Get interesting date from the log record.
finish_time(#wm_log_data{finish_time=FinishTime}) -> FinishTime.
method(#wm_log_data{method=Method}) -> Method.
path(#wm_log_data{path=Path}) -> Path.
version(#wm_log_data{version=Version}) -> Version.
response_code(#wm_log_data{response_code=ResponseCode}) -> ResponseCode.
peer(#wm_log_data{peer=Peer}) -> Peer.

% @doc Get the referrer.
referrer(#wm_log_data{headers=Headers}) ->
    get_header_value("Referer", Headers, "").

% @doc Get the user agent.
user_agent(#wm_log_data{headers=Headers}) ->
    get_header_value("User-Agent", Headers, "").

% @doc Get the user agent.
response_length(#wm_log_data{response_length=ResponseLength}) ->
    case is_atom(ResponseLength) of
        true -> 
            atom_to_list(ResponseLength);
        false -> 
            integer_to_list(ResponseLength)
    end.

get_header_value(Name, Headers, Default) ->
    case mochiweb_headers:get_value(Name, Headers) of
         undefined -> Default;
         R -> R
    end.

