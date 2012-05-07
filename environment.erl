%%%---------------------------------------------------------------------
%%% Description module environment
%%%---------------------------------------------------------------------
%%% The environment, which uses the behaviour gen_server to provide the
%%% functionality and interfaces
%%%---------------------------------------------------------------------
%%% Exports
%%%---------------------------------------------------------------------
%%% start()
%%%   Initializes the environment with a new population based on
%%%   default values
%%% start(Alphabet, Size, Population, Mutation, Fitness, Options)
%%%   Initializes the environment with a new population
%%% stop()
%%%   Stops the environment
%%% state()
%%%   Returns internal states
%%% fitness()
%%%   Returns the fitness of each individual
%%% gene_pool()
%%%   Returns all genomes of the population
%%% tick(Count)
%%%   Reuest and initializes the next Count cycles.
%%% init([Alphabet, Size, Population, Mutation, Fitness])
%%%   Interface for the behaviour gen_server.
%%% handle_cast({reproduced, Parent, Childs}, From, State)
%%%   Interface for the behaviour gen_server. Handles the result of a
%%%   reproduction or mutation.
%%% handle_call(state, From, State)
%%%   Interface for the behaviour gen_server. Returns internal states
%%% handle_call(fitness, From, State)
%%%   Interface for the behaviour gen_server. Returns the fitness of
%%%   each individual
%%% handle_cast(tick, From, State)
%%%   Interface for the behaviour gen_server. Initializes the next
%%%   cycle, if requested and the last one finished
%%% handle_cast({ticks, Count}, From, State)
%%%   Interface for the behaviour gen_server. Request and initializes
%%%   the next Count cycles
%%% handle_cast({dead, Individual}, State)
%%%   Interface for the behaviour gen_server. Handles death
%%%   individuals
%%% handle_cast(stop, State)
%%%   Interface for the behaviour gen_server. Kills the environment
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

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).
-export([start/0, start/6, stop/0, state/0, fitness/0, gene_pool/0,
	tick/1]).

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
	%%%% 1111.. wins
	
	Fitness = fun(State) ->
		{individualState, _, _, Genome, _} = State,
		
		FunMap = fun(X) ->
			integer_to_list(X)
		end,
		Decimal = list_to_integer(
			string:join(lists:map(FunMap, Genome), "")
			, 2),
		
		Decimal * Decimal
	end,
	
	start({0,1}, 8, 100, 0.2, Fitness, [crossover]).

%%----------------------------------------------------------------------
%% Function: start/6
%% Purpose: Calls gen_server:start_link/3 with alphabet, size the
%%          population amount, the mutation rate and a fittnes function.
%%          Register the gen_server to the global name environment
%% Args: Alphabet, the possible alphabet for the genomes
%%       and Size, the size of the genomes of the new individuals
%%       and Population, the size fo the new population
%%       and Mutation, the mutation rate (0.0 .. 1.0)
%%       and Fitness, a fun for the fitness
%%       and Options, as a list of atoms which define the behaviour of
%%         the genetic algorithm
%% Returns: {ok,Pid} | ignore | {error,Error}
%%----------------------------------------------------------------------
start(Alphabet, Size, Population, Mutation, Fitness, Options)
	when is_tuple(Alphabet), is_function(Fitness), is_float(Mutation),
	is_list(Options), Size > 0, Population > 0, Mutation >= 0.0,
	Mutation =< 1.0 ->
	
	gen_server:start_link({global, environment}, ?MODULE, [Alphabet, Size,
		Population, Mutation, Fitness, Options], []).
  
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
%% Function: fitness/0
%% Purpose: Returns the fitness of each individual
%% Args: -
%% Returns: {fitness, Fitness}
%%----------------------------------------------------------------------
fitness() -> gen_server:call({global, environment}, fitness).

%%----------------------------------------------------------------------
%% Function: fitness/1
%% Purpose: Returns the fitness of each individual
%% Args: State, the environment State
%% Returns: {fitness, Fitness}
%%----------------------------------------------------------------------
fitness(State) ->
	FunMap = fun(Individual) ->
		{_Pid, Fitness} = Individual,
		Fitness
	end,
	Fitness = lists:map(FunMap, State#environmentState.population),
	
	{fitness, Fitness}.

%%----------------------------------------------------------------------
%% Function: gene_pool/0
%% Purpose: Returns all genomes of the population
%% Args: -
%% Returns: {gene_pool, [Genomes]}.
%%----------------------------------------------------------------------
gene_pool() ->
	{state, State} = gen_server:call({global, environment}, state),
	
	FunMap = fun(Individual) ->
		{Pid, _Fitness} = Individual,
		{state, IState} = individual:state(Pid),
		IState#individualState.genome
	end,
	Genomes = lists:map(FunMap, State#environmentState.population),
	
	{gene_pool, Genomes}.

%%----------------------------------------------------------------------
%% Function: tick/1
%% Purpose: Request and initializes the next Count cycles.
%% Args: Count, the amount of cycles to request
%% Returns: ok.
%%----------------------------------------------------------------------
tick(Count) when is_integer(Count), Count > 0 ->
	gen_server:cast({global, environment}, {ticks, Count}).

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
%% Function: init/1
%% Purpose: Initializes the environment with a new population based on
%%          the given Alphabet
%% Args: Alphabet, the possible alphabet for the genomes
%%       and Size, the size of the genomes of the new individuals
%%       and Population, the size of the new population
%%       and Mutation, the mutation rate (0.0 .. 1.0)
%%       and Fitness, a fun for the fitness
%%       and Options, as a list of atoms which define the behaviour of
%%         the genetic algorithm
%% Returns: {ok, State}.
%%----------------------------------------------------------------------
init([Alphabet, Size, Population, Mutation, Fitness, Options])
	when is_tuple(Alphabet), is_integer(Size), is_integer(Population),
	is_float(Mutation), is_function(Fitness), size(Alphabet) > 0,
	Size > 0, Population > 0, Mutation >= 0.0, Mutation =< 1.0 ->
	
	{A1,A2,A3} = now(),
	random:seed(A1, A2, A3),
	
	{population, Pop} = create_population(Alphabet, Size, Fitness,
		Population),
	
	DefaultState = #environmentState{mutation = Mutation, population = Pop},
	{state, State} = evaluate_options(Options, DefaultState),
	
	gen_server:cast({global, monitor}, {population, 0, Pop}),

	{ok, State}.

%%----------------------------------------------------------------------
%% Function: evaluate_options/2
%% Purpose: Evaluates the given options and set the corresponding state
%%          flags
%% Args: Options, the given list with options
%%       and State, the actual state
%% Returns: {state, NewState}.
%%----------------------------------------------------------------------
evaluate_options(Options, State) when is_list(Options) ->
	FunCrossover = fun(X) ->
		X == crossover
	end,
	
	Crossover = lists:any(FunCrossover, Options),
	
	State1 = State#environmentState{crossover = Crossover},
	
	{state, State1}.

%%----------------------------------------------------------------------
%% Function: handle_call/3
%% Purpose: Returns internal state
%% Args: -
%% Returns: {reply, {state, State}, State}.
%%----------------------------------------------------------------------
handle_call(state, _From, State) -> {reply, {state, State}, State};

%%----------------------------------------------------------------------
%% Function: handle_call/3
%% Purpose: Returns the fitness of each individual
%% Args: -
%% Returns: {reply, {fitness, Fitness}, State}.
%%----------------------------------------------------------------------
handle_call(fitness, _From, State) ->
	{fitness, Fitness} = fitness(State),
	{reply, {fitness, Fitness}, State}.

%%----------------------------------------------------------------------
%% Function: internal_tick/1
%% Purpose: Initializes the next cycle
%% Args: State, the actual State
%% Returns: {ok, NewState}.
%%----------------------------------------------------------------------
internal_tick(State) ->
	% sort with fitness
	FunSort = fun(A, B) ->
		element(2,A) >= element(2,B)
	end,
	
	SortedPopulation = lists:sort(FunSort,
		State#environmentState.population),
	
	% reproduce good ones and kill bad ones
	Length = round(length(SortedPopulation)/2),
	{AllGoodOnes, BadOnes} = lists:split(Length, SortedPopulation),
	
	% fun - trigger clone
	FunClone = fun(Individual) ->
		{Pid, _} = Individual,
		
		gen_server:cast(Pid, {clone,
			State#environmentState.mutation})
	end,
	
	%% this implementation of crossover can only handle populations
	%% which are dividable by 4. as workaround we clone the last one
	%% of the good ones and only crossover the other good ones
	if
		State#environmentState.crossover =:= true ->
			if
				Length rem 2 == 1 ->
					Individual = lists:last(AllGoodOnes),
					GoodOnes = lists:delete(Individual, AllGoodOnes),
					FunClone(Individual);
				true ->
					GoodOnes = AllGoodOnes
			end;
		true ->
			GoodOnes = AllGoodOnes
	end,
	
	% fun - trigger crossover
	%% only odd ones will be triggered
	FunCrossover = fun(N) ->
		{Pid, _} = lists:nth(N, GoodOnes),
		
		% select partner
		if
			N rem 2 == 1 ->
				{Partner, _} = lists:nth(N+1, GoodOnes),
				gen_server:cast(Pid, {crossover,
					State#environmentState.mutation, Partner});
			true ->
				true
		end
	end,
	
	% fun - trigger stop
	FunKill = fun(Individual) ->
		{Pid, _} = Individual,
		gen_server:cast(Pid, stop)
	end,
	
	% call triggers
	lists:foreach(FunKill, BadOnes),
	if
		State#environmentState.crossover ->
			lists:foreach(FunCrossover, lists:seq(1, length(GoodOnes)));
		true ->
			lists:foreach(FunClone, GoodOnes)
	end,
	
	% update state
	NewState = State#environmentState{
		population = GoodOnes,
		ticks = State#environmentState.ticks - 1
	},
	
	{ok, NewState}.

%%----------------------------------------------------------------------
%% Function: finish_tick/1
%% Purpose: Check if the actual tick is finished and clean up.
%%          Sends additionally a call to a global monitor, which can
%%          then generate some statistics
%% Args: State, the actual State
%% Returns: {ok, NewState}.
%%----------------------------------------------------------------------
finish_tick(State) ->
	if
		length(State#environmentState.population) == 0 ->
			% update state
			NewState = State#environmentState{
				population = State#environmentState.new_population,
				new_population = [],
				killlist = [],
				age = State#environmentState.age + 1
			},
			
			% request next tick
			gen_server:cast(self(), tick),
			
			% inform monitor
			gen_server:cast({global, monitor}, {population,
				NewState#environmentState.age,
				NewState#environmentState.population}),
			
			{ok, NewState};
		true ->
			{ok, State}
	end.

%%----------------------------------------------------------------------
%% Function: handle_cast/2
%% Purpose: Initializes the next cycle if there are more ticks requested
%%          and the last one is finished
%% Args: -
%% Returns: {noreply, NewState}.
%%----------------------------------------------------------------------
handle_cast(tick, State) ->
	if
		State#environmentState.ticks > 0,
		length(State#environmentState.new_population) == 0 ->
			{ok, NewState} = internal_tick(State),
			{noreply, NewState};
		true ->
			{noreply, State}
	end;

%%----------------------------------------------------------------------
%% Function: handle_cast/2
%% Purpose: Request the next Count cycles
%% Args: -
%% Returns: {noreply, NewState}.
%%----------------------------------------------------------------------
handle_cast({ticks, Count}, State) ->
	NewState =	State#environmentState {
		ticks = State#environmentState.ticks + Count
	},
	
	gen_server:cast(self(), tick),
	
	{noreply, NewState};

%%----------------------------------------------------------------------
%% Function: handle_cast/2
%% Purpose: Handles incoming reproduction results
%% Args: -
%% Returns: {noreply, NewState}.
%%----------------------------------------------------------------------
handle_cast({reproduced, Parent, Childs}, State) ->
	StateNewPopulation = State#environmentState.new_population,
	NewPopulation = StateNewPopulation ++ Childs,

	NewState = State#environmentState{
		new_population = NewPopulation
	},
	
	gen_server:cast(Parent, stop),
	
	{ok, NewState2} = finish_tick(NewState),
	
	{noreply, NewState2};

%%----------------------------------------------------------------------
%% Function: handle_cast/2
%% Purpose: Handles incoming reproduction results
%% Args: -
%% Returns: {noreply, NewState}.
%%----------------------------------------------------------------------
%handle_cast({killme, Individual}, State) ->
%	FunFilter = fun(Ind) ->
%		{Pid, _Fitness} = Ind,
%		if
%			Pid == Individual ->
%				false;
%			true ->
%				true
%		end
%	end,
%	Population = lists:filter(FunFilter, State#environmentState.population),
%	
%	Killlist = State#environmentState.killlist,
%	
%	NewState = State#environmentState{
%		population = Population,
%		killlist = Killlist ++ [Individual]
%	},
%	
%	{ok, NewState2} = finish_tick(NewState),
%	
%	{noreply, NewState2};

%%----------------------------------------------------------------------
%% Function: handle_cast/2
%% Purpose: Handles incoming reproduction results
%% Args: -
%% Returns: {noreply, NewState}.
%%----------------------------------------------------------------------
handle_cast({dead, Individual}, State) ->
	FunFilter = fun(Ind) ->
		{Pid, _Fitness} = Ind,
		if
			Pid == Individual ->
				false;
			true ->
				true
		end
	end,
	Population = lists:filter(FunFilter, State#environmentState.population),
	
	NewState = State#environmentState{
		population = Population
	},
	
	{ok, NewState2} = finish_tick(NewState),
	
	{noreply, NewState2};

%%----------------------------------------------------------------------
%% Function: handle_cast/2
%% Purpose: Stops the environment
%% Args: -
%% Returns: {stop, normal, State}.
%%----------------------------------------------------------------------
handle_cast(stop, State) ->
	% fun - trigger stop
	FunKill = fun(Individual) ->
		{Pid, _} = Individual,
		gen_server:cast(Pid, stop)
	end,
	
	% call triggers
	lists:foreach(FunKill, State#environmentState.population),
	lists:foreach(FunKill, State#environmentState.new_population),
	
	{stop, normal, State}.

%%----------------------------------------------------------------------
%% Function: *
%% Purpose: Dummy functions for the behaviour gen_server
%%----------------------------------------------------------------------
terminate(normal, _State) -> ok;
terminate(_Reason, _State) -> ok.
handle_info(_Message, State) -> {noreply, State}.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
