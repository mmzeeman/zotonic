%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2013 Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%%
%% @doc Pool support for modules/services which need long running managed processes. 

-module (z_pool).

-include("include/zotonic.hrl").

-export([
    start/3, start/4, 
    stop/2, 
    run/3,
    queue_wait/3, queue_wait/4,
    queue/3,
    status/2
    ]).

% @doc Start a new process pool by passing it the mfa of the processes 
% which will be started. 
%
% @spec start(atom(), mfa(), Context) -> ok | {error, _}.
start(Name, MFA, Context) ->
    start(Name, MFA, 100, Context).
%
% @doc Start a new process pool by passing it the mfa of the processes 
% which will be started. 
%
% @spec start(atom(), mfa(), nat(), Context) -> ok | {error, _}.
start(Name, MFA, Limit, Context) ->
    tubby:start(tubby_name(Name, Context), MFA, Limit).

% @doc Stop a pool and all its registred processes.
% @spec stop(atom(), Contex) -> _.
stop(Name, Context) ->
    tubby:stop(tubby_name(Name, Context)).

% @doc Run task on pool Name.
%
run(Name, Args, Context) ->
    tubby:run(tubby_name(Name, Context), Args).

% @doc Queue a task on pool Name, and wait for it to start.
queue_wait(Name, Args, Context) ->
    tubby:queue_wait(tubby_name(Name, Context), Args).

% @doc Queue a task on pool Name, and wait for it to start or timeout when
%   the waiting takes too long.
queue_wait(Name, Args, Timeout, Context) ->
    tubby:queue_wait(tubby_name(Name, Context), Timeout, Args).

% @doc Queue task on pool Name and continue immediately.
queue(Name, Args, Context) ->
    tubby:queue(tubby_name(Name, Context), Args).

% @doc Return the current status of the pool. Returns the
% number of running tasks, the number of waiting tasks and 
% the current limit.
status(Name, Context) ->
    tubby:status(tubby_name(Name, Context)).

% @doc Get the registry name.
%
% TODO: when R15 is the minimum release we could use reggy to register the
% pool with the via trick. For now, build an atom.
tubby_name(Name, #context{host=Host}) ->
    tubby_name(Name, Host);
tubby_name(Name, Host) ->
    z_utils:name_for_host(Name, z_utils:name_for_host(tubby, Host)).

