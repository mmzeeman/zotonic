% Basic webmachine resource, copied from webmachine.

-export([start_link/1]).
-export([ping/2]).

start_link(Args) ->
    webmachine_controller:start_link(?MODULE, [Args]).

ping(ReqData, State) ->
	%% Experimental flushing all messages from the mailbox.
	flush_messages(),
    {pong, ReqData, State}.

flush_messages() ->
	receive 
		_Msg -> flush_messages()
	after 
		0 -> ok
	end.