%%---------------------------------------------------------------------
%% Data Type: individualState
%% where:
%%    alphabet: A tuple (default is undefined).
%%    fitness: A function (default is udnefined).
%%    genome: A list with values of alphabet (default is undefined).
%%    age: An integer (default is 0).
%%----------------------------------------------------------------------
-record(individualState, {alphabet, fitness, genome, age = 0}).

%%---------------------------------------------------------------------
%% Data Type: environmentState
%% where:
%%    population: A list with individuals (default is []).
%%    new_population: A list with individuals (default is []).
%%    mutation: An flot (default is 0.0).
%%    age: An integer (default is 0).
%%    ticks: An integer (default is 0).
%%----------------------------------------------------------------------
-record(environmentState, {population = [], new_population = [],
	mutation = 0.0, age = 0, ticks = 0}).

%%---------------------------------------------------------------------
%% Data Type: monitorState
%% where:
%%    filename: A String (default is undefined).
%%----------------------------------------------------------------------
-record(monitorState, {filename}).
