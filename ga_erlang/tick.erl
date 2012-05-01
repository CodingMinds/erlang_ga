#!/usr/bin/env escript
%%! -name ticker

main([String]) ->
	% connect to other nodes and sync global names
	net_adm:world(),
	global:sync(),
	
	% send tick or print usage, if cmd arg bad
	try
		Count = list_to_integer(String),
		environment:tick(Count)
	catch
		_:_ ->
			usage()
	end,
	
	wait();

% default: print usage
main(_) ->
	usage().

% wait/loop until all ticks proceed
wait() ->
	{state, {environmentState, _, _, _, _, Ticks}}
		= environment:state(),
	
	if
		Ticks == 0 ->
			true;
		true ->
			io:format(standard_error, "Waiting, ~w Ticks to proceed\n", [Ticks]),
			
			receive
				after 500 -> ok
			end,
			wait()
	end.

% print usage and die
usage() ->
	io:format(standard_error, "usage: tick.erl Count\n", []),
	halt(1).
