%%%---------------------------------------------------------------------
%%% Description module environment
%%%---------------------------------------------------------------------
%%% The environment , which uses the behaviour gen_server to provide the
%%% functionality and interfaces
%%%---------------------------------------------------------------------
%%% Exports
%%%---------------------------------------------------------------------
%%% start()
%%%   Initializes the environment with a new population based on
%%%   default values
%%% start(Alphabet, Size, Population, Mutation, Fitness)
%%%   Initializes the environment with a new population
%%% stop()
%%%   Stops the environment
%%% state()
%%%   Returns internal states.
%%% tick()
%%%   Initializes the next cycle.
%%% tick(Count)
%%%   Initializes the next Count cycles.
%%% init([Alphabet, Size, Population, Mutation, Fitness])
%%%   Interface for the behaviour gen_server.
%%% handle_cast({reproduced, IState, Childs}, From, State)
%%%   Interface for the behaviour gen_server. Handles the result of a
%%%   reproduction or mutation.
%%% handle_call(state, From, State)
%%%   Interface for the behaviour gen_server. Returns internal states
%%% handle_cast(tick, From, State)
%%%   Interface for the behaviour gen_server. Initializes the next cycle
%%% handle_cast({ticks, Count}, From, State)
%%%   Interface for the behaviour gen_server. Initializes the next Count
%%%    cycles
%%% handle_cast(stop, State)
%%%   Interface for the behaviour gen_server. Kills the individual
%%% handle_info(Message, State)
%%%   Interface for the behaviour gen_server. Dummy w/o functionality
%%% terminate(Reason, State)
%%%   Interface for the behaviour gen_server. Kills the individual
%%% code_change(OldVersion, State, Extra)
%%%   Interface for the behaviour gen_server. Dummy w/o functionality
%%%---------------------------------------------------------------------

-module(environment).
-author('GEEK1 <erlang@geek1.de>').
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/0, start/5, stop/0, state/0, tick/0, tick/1]).

-include("records.hrl").

%%----------------------------------------------------------------------
%% Function: start/0
%% Purpose: Wrapper for start/3 with default values
%% Args: -
%% Returns: {ok,Pid} | ignore | {error,Error}
%%----------------------------------------------------------------------
start() ->
	%%%% TODO:
	%%%% Implement a really better first fitness function
	Fitness = fun(State) ->
		State#individualState.age
	end,
	
	start({a,b,c}, 7, 12, 10, Fitness).

%%----------------------------------------------------------------------
%% Function: start/5
%% Purpose: Calls gen_server:start_link/3 with alphabet, size the
%%          population amount, the mutation rate and a fittnes function.
%%          Register the gen_server to the global name environment
%% Args: Alphabet, the possible alphabet for the genomes
%%       and Size, the size of the genomes of the new individuals
%%       and Population, the size fo the new population
%%       and Mutation, the mutation rate
%%       and Fitness, a fun for the fitness
%% Returns: {ok,Pid} | ignore | {error,Error}
%%----------------------------------------------------------------------
start(Alphabet, Size, Population, Mutation, Fitness)
	when is_tuple(Alphabet), is_function(Fitness), Size > 0,
	Population > 0 ->
	
	gen_server:start_link({global, environment}, ?MODULE, [Alphabet, Size,
		Population, Mutation, Fitness], []).
  
%%----------------------------------------------------------------------
%% Function: stop/0
%% Purpose: Stops the environment
%% Args: -
%% Returns: ok.
%%----------------------------------------------------------------------
stop() -> gen_server:cast({global, environment}, stop).

%%----------------------------------------------------------------------
%% Function: state/0
%% Purpose: Returns internal state
%% Args: -
%% Returns: {state, State}
%%----------------------------------------------------------------------
state() -> gen_server:call({global, environment}, state).

%%----------------------------------------------------------------------
%% Function: tick/0
%% Purpose: Initializes the next cycle.
%% Args: -
%% Returns: ok.
%%----------------------------------------------------------------------
tick() -> gen_server:cast({global, environment}, tick).

%%----------------------------------------------------------------------
%% Function: tick/1
%% Purpose: Initializes the next cycle.
%% Args: Count, the amount of cycles to proceed
%% Returns: ok.
%%----------------------------------------------------------------------
tick(Count) when is_integer(Count), Count > 0 ->
	gen_server:cast({global, environment}, {ticks, Count}).

%%----------------------------------------------------------------------
%% Function: init/1
%% Purpose: Initializes the environment with a new population based on
%%          the given Alphabet
%% Args: Alphabet, the possible alphabet for the genomes
%%       and Size, the size of the genomes of the new individuals
%%       and Population, the size of the new population
%%       and Fitness, a fun for the fitness
%% Returns: {ok, State}.
%%----------------------------------------------------------------------
init([Alphabet, Size, Population, Mutation, Fitness])
	when is_tuple(Alphabet), is_integer(Size), is_integer(Population),
	is_integer(Mutation), is_function(Fitness), size(Alphabet) > 0,
	Size > 0, Population > 0, Mutation >= 0 ->
	
	{A1,A2,A3} = now(),
	random:seed(A1, A2, A3),
	
	{population, Pop} = create_population(Alphabet, Size, Fitness,
		Population),
	
	State = #environmentState{mutation = Mutation, population = Pop},
	
	{ok, State}.

%%----------------------------------------------------------------------
%% Function: create_population/4
%% Purpose: A wrapper for create_population/4, which creates a new
%%          population
%% Args: Alphabet, the possible alphabet for the genomes
%%       and Size, the size of the genomes of the new individuals
%%       and Fitness, the fitness fun for the new individuals
%%       and Population, the size of the new population
%% Returns: {population, [Population]}.
%%----------------------------------------------------------------------
create_population(Alphabet, Size, Fitness, Population)
	when is_tuple(Alphabet), is_integer(Size), is_function(Fitness),
	is_integer(Population), Size > 0, size(Alphabet) > 0,
	Population > 0 ->
	
	create_population(Alphabet, Size, Fitness, Population, []).

%%----------------------------------------------------------------------
%% Function: create_population/4
%% Purpose: Creates tail recursive a new population
%% Args: Alphabet, the possible alphabet for the genomes
%%       and Size, the size of the genomes of the new individuals
%%       and Fitness, the fitness fun for the new individuals
%%       and Population, the size of the new population
%%       and Acc, as a accumulator for the new individuals
%% Returns: {population, [Population]}.
%%----------------------------------------------------------------------
create_population(Alphabet, Size, Fitness, Population, Acc)
	when is_tuple(Alphabet), is_integer(Size), is_function(Fitness),
	is_integer(Population), is_list(Acc), Size > 0, size(Alphabet) > 0,
	Population > 0 ->
	
	{ok, Pid} = individual:start(random, Alphabet, Fitness, Size),
	{fitness, Fit} = individual:fitness(Pid),
	
	create_population(Alphabet, Size, Fitness, Population - 1, Acc ++ [{Pid, Fit}]);

create_population(_Alphabet, _Size, _Fitness, 0, Acc) ->
	{population, Acc}.

%%----------------------------------------------------------------------
%% Function: handle_call/3
%% Purpose: Returns internal state
%% Args: -
%% Returns: {reply, {state, State}, State}.
%%----------------------------------------------------------------------
handle_call(state, _From, State) -> {reply, {state, State}, State}.

%%----------------------------------------------------------------------
%% Function: handle_cast/2
%% Purpose: Initilaizes the next cycle
%% Args: -
%% Returns: {noreply, NewState}.
%%----------------------------------------------------------------------
handle_cast(tick, State) ->
	
	% sort with fitness
	FunSort = fun(A, B) ->
		element(2,A) =< element(2,B)
	end,
	
	SortedPopulation = lists:sort(FunSort,
		State#environmentState.population),
	
	% reproduce good ones and kill bad ones
	{Reproduce, Kill} = lists:split(round(length(SortedPopulation)/2),
		SortedPopulation),
	
	FunReproduce = fun(Individual) ->
		{Pid, _} = Individual,
		%% TODO:
		%% Handle mutation !
		gen_server:cast(Pid, reproduce)
	end,
	FunKill = fun(Individual) ->
		{Pid, _} = Individual,
		gen_server:cast(Pid, stop)
	end,
	
	lists:foreach(FunReproduce, Reproduce),
	lists:foreach(FunKill, Kill),
	
	NewState = State#environmentState{
		population = Reproduce,
		ticks = State#environmentState.ticks + 1
	},
	
	{noreply, NewState};

%%----------------------------------------------------------------------
%% Function: handle_cast/2
%% Purpose: Handles incoming reproduction results
%% Args: -
%% Returns: {noreply, NewState}.
%%----------------------------------------------------------------------
handle_cast({reproduced, _ParentAlive, Parent, Childs}, State) ->
	%%%% TODO:
	%%%% handle if parent is death or not .. now we guess it's death.
	
	NewState = State#environmentState{
		population = State#environmentState.population -- [Parent],
		new_population = 	State#environmentState.new_population ++ Childs
	},
	
	if
		length(State#environmentState.population) == 0 ->
			NewState2 = State#environmentState{
				population = State#environmentState.new_population,
				new_population = [],
				age = State#environmentState.age + 1,
				ticks = State#environmentState.ticks - 1
			},
			
			{noreply, NewState2};
		true ->
			{noreply, NewState}
	end;
	

%%----------------------------------------------------------------------
%% Function: handle_cast/2
%% Purpose: Stops the environment
%% Args: -
%% Returns: {stop, normal, State}.
%%----------------------------------------------------------------------
handle_cast(stop, State) -> {stop, normal, State}.

%%----------------------------------------------------------------------
%% Function: *
%% Purpose: Dummy functions for the behaviour gen_server
%%----------------------------------------------------------------------
terminate(normal, _State) -> ok;
terminate(_Reason, _State) -> ok.
handle_info(_Message, State) -> {noreply, State}.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
