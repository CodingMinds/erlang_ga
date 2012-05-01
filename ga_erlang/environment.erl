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
%%%   Returns internal states
%%% stats()
%%%   Returns the fitness of each individual
%%% tick(Count)
%%%   Reuest and initializes the next Count cycles.
%%% init([Alphabet, Size, Population, Mutation, Fitness])
%%%   Interface for the behaviour gen_server.
%%% handle_cast({reproduced, IState, Childs}, From, State)
%%%   Interface for the behaviour gen_server. Handles the result of a
%%%   reproduction or mutation.
%%% handle_call(state, From, State)
%%%   Interface for the behaviour gen_server. Returns internal states
%%% handle_cast(tick, From, State)
%%%   Interface for the behaviour gen_server. Initializes the next cycle,
%%%   if requested and the last one finished
%%% handle_cast({ticks, Count}, From, State)
%%%   Interface for the behaviour gen_server. Request and initializes the
%%%   next Count cycles
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
-export([start/0, start/5, stop/0, state/0, stats/0, tick/1]).

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
	%%%% cccc.. wins
	Fitness = fun(State) ->
		FunMap = fun(X) ->
			if
				X == a -> 1;
				X == b -> 2;
				X == c -> 3
			end
		end,
		MappedGenome = lists:map(FunMap, State#individualState.genome),
		lists:sum(MappedGenome)
	end,
	
	start({a,b,c}, 7, 12, 0.2, Fitness).

%%----------------------------------------------------------------------
%% Function: start/5
%% Purpose: Calls gen_server:start_link/3 with alphabet, size the
%%          population amount, the mutation rate and a fittnes function.
%%          Register the gen_server to the global name environment
%% Args: Alphabet, the possible alphabet for the genomes
%%       and Size, the size of the genomes of the new individuals
%%       and Population, the size fo the new population
%%       and Mutation, the mutation rate (0.0 .. 1.0)
%%       and Fitness, a fun for the fitness
%% Returns: {ok,Pid} | ignore | {error,Error}
%%----------------------------------------------------------------------
start(Alphabet, Size, Population, Mutation, Fitness)
	when is_tuple(Alphabet), is_function(Fitness), is_float(Mutation),
	Size > 0, Population > 0, Mutation >= 0.0, Mutation =< 1.0 ->
	
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
%% Function: stats/0
%% Purpose: Returns the fitness of each individual
%% Args: -
%% Returns: {state, State}
%%----------------------------------------------------------------------
stats() -> gen_server:call({global, environment}, state).

%%----------------------------------------------------------------------
%% Function: tick/1
%% Purpose: Request and initializes the next Count cycles.
%% Args: Count, the amount of cycles to request
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
%%       and Mutation, the mutation rate (0.0 .. 1.0)
%%       and Fitness, a fun for the fitness
%% Returns: {ok, State}.
%%----------------------------------------------------------------------
init([Alphabet, Size, Population, Mutation, Fitness])
	when is_tuple(Alphabet), is_integer(Size), is_integer(Population),
	is_float(Mutation), is_function(Fitness), size(Alphabet) > 0,
	Size > 0, Population > 0, Mutation >= 0.0, Mutation =< 1.0 ->
	
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
handle_call(state, _From, State) -> {reply, {state, State}, State};

%%----------------------------------------------------------------------
%% Function: handle_call/3
%% Purpose: Returns the fitness of each individual
%% Args: -
%% Returns: {reply, {stats, Stats}, State}.
%%----------------------------------------------------------------------
handle_call(stats, _From, State) ->
	FunMap = fun(Individual) ->
		{_Pid, Fitness} = Individual,
		Fitness
	end,
	Fitness = lists:map(FunMap, State#environmentState.population),
	
	{reply, {stats, Fitness}, State}.

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
	{Reproduce, Kill} = lists:split(round(length(SortedPopulation)/2),
		SortedPopulation),
	
	% fun - trigger reproduction
	FunReproduce = fun(Individual) ->
		{Pid, _} = Individual,
		Mutation = random:uniform(),
		if
			Mutation =< State#environmentState.mutation ->
				gen_server:cast(Pid, mutate);
			true ->
				gen_server:cast(Pid, reproduce)
		end
	end,
	
	% fun - trigger stop
	FunKill = fun(Individual) ->
		{Pid, _} = Individual,
		gen_server:cast(Pid, stop)
	end,
	
	% call triggers
	lists:foreach(FunReproduce, Reproduce),
	lists:foreach(FunKill, Kill),
	
	NewState = State#environmentState{
		population = Reproduce,
		ticks = State#environmentState.ticks - 1
	},
	
	{ok, NewState}.

%%----------------------------------------------------------------------
%% Function: finish_cycle/1
%% Purpose: Check if the actual cycle is finished and clean up
%% Args: State, the actual State
%% Returns: {ok, NewState}.
%%----------------------------------------------------------------------
finish_cycle(State) ->
	if
		length(State#environmentState.population) == 0 ->
			NewState = State#environmentState{
				population = State#environmentState.new_population,
				new_population = [],
				age = State#environmentState.age + 1
			},
			
			gen_server:cast({global, environment}, tick),
			
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
	
	gen_server:cast({global, environment}, tick),
	
	{noreply, NewState};

%%----------------------------------------------------------------------
%% Function: handle_cast/2
%% Purpose: Handles incoming reproduction results
%% Args: -
%% Returns: {noreply, NewState}.
%%----------------------------------------------------------------------
handle_cast({reproduced, _Parent, Childs}, State) ->
	%%%% TODO: y? {badrecord,environmentState} if State..-- or State..++
	FunMap = fun(Child) ->
		{fitness, Fit} = individual:fitness(Child),
		{Child, Fit}
	end,
	
	Childs2 = lists:map(FunMap, Childs),
	
	StateNewPopulation = State#environmentState.new_population,
	NewPopulation = StateNewPopulation ++ Childs2,

	NewState = State#environmentState{
		new_population = NewPopulation
	},
	
	{ok, NewState2} = finish_cycle(NewState),
	
	{noreply, NewState2};

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
	
	{ok, NewState2} = finish_cycle(NewState),
	
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
