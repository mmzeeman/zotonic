%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2013 Maas-Maarten Zeeman
%% @doc Start the engine.io connection.

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

-module(scomp_base_eio).
-behaviour(gen_scomp).

-export([vary/2, render/3]).

-include("zotonic.hrl").

vary(_Params, _Context) -> nocache.

render(_Params, _Vars, Context) ->
    Script = engine_io_script(Context),
    {ok, z_script:add_script(Script, Context)}.

% Make the call of the start script.
engine_io_script(Context) ->
    [<<"z_engine_start(">>, add_subdomain(z_context:streamhost(Context)), ");"].
    
% Add random number 0-9
add_subdomain([$?|Hostname]) ->
    [$', integer_to_list(z_ids:number(10)), Hostname, $'];
add_subdomain(<<$?,Hostname/binary>>) ->
    [$', integer_to_list(z_ids:number(10)), Hostname, $'];

% Add random number, no real limits
add_subdomain([$*|Hostname]) ->
    [$', integer_to_list(z_ids:number()),Hostname,$'];
add_subdomain(<<$*,Hostname/binary>>) ->
    [$',integer_to_list(z_ids:number()), Hostname, $'];
add_subdomain([$.|_] = Hostname) ->
    [$',integer_to_list(z_ids:number()), Hostname, $'];
add_subdomain(<<$.,_/binary>> = Hostname) ->
    [$', integer_to_list(z_ids:number()), Hostname, $'];
    
% special case for the zotonic_status site
add_subdomain(none) ->
    "window.location.host";
    
% Just connect to the hostname itself
add_subdomain(Hostname) ->
    [$', Hostname, $'].
