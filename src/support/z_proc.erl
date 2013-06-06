%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2013 Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%%
%% @doc Register site specific processes. 

-module (z_proc).

-include("include/zotonic.hrl").

-export([register/3, unregister/2, whereis/2]).

% @doc Register a Pid under Name. Name can be any erlang term.
%
% @spec reg(term(), pid(), Context) -> ok | {error, _}.
register(Name, Pid, Context) ->
    reggy_reg:register(registry(Context), Name, Pid).


% @doc Unregister a process
% @spec unreg(term(), Contex) -> _.
unregister(Name, Context) ->
    reggy_reg:unregister(registry(Context), Name).

% @doc Lookup the pid of the process.
%
% @spec where(term(), Context) -> pid() | undefined.
whereis(Name, Context) ->
    reggy_reg:whereis(registry(Context), Name).


% @doc Get the registry name.
registry(#context{host=Host}) ->
    registry(Host);
registry(Host) ->
    z_utils:name_for_host(reggy, Host).

