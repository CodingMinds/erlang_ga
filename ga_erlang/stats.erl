#!/usr/bin/env escript
%%! -name stats

main([]) ->
	% connect to other nodes and sync global names
	net_adm:world(),
	global:sync(),
	
	{stats, Fitness} = environment:stats(),
	
	FunFor = fun(X) ->
		io:format("~w~n", [X])
	end,
	lists:foreach(FunFor, Fitness);

main(["short"]) ->
	% connect to other nodes and sync global names
	net_adm:world(),
	global:sync(),
	
	{stats, Fitness} = environment:stats(),
	
	io:format("~w~n", [lists:sum(Fitness)]);

% default: print usage
main(_) ->
	usage().

% print usage and die
usage() ->
	io:format(standard_error, "usage: stats.erl [short]\n", []),
	halt(1).
