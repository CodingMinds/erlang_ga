%%%---------------------------------------------------------------------
%%% Description module individual
%%%---------------------------------------------------------------------
%%% A individual, which uses the behaviour gen_server to provide the
%%% functionality and interfaces
%%%---------------------------------------------------------------------
%%% Exports
%%%---------------------------------------------------------------------
%%% start(predefined, Alphabet, Fitness, Genome)
%%%   Initializes a new individual with a predefined genome
%%% start(random, Alphabet, Fitness, Size)
%%%   Initializes a new individual with size Size and a random genome
%%%   build with the given Alphabet
%%% stop(Pid)
%%%   Kills the individual
%%% state(Pid)
%%%   Returns internal states.
%%% fitness(Pid)
%%%   Returns the actual fitness.
%%% init([predefined, Alphabet, Fitness, Genome])
%%%   Interface for the behaviour gen_server.
%%% init([random, Alphabet, Fitness, Size])
%%%   Interface for the behaviour gen_server.
%%% handle_cast({reproduce, Mutation}, State)
%%%   Interface for the behaviour gen_server. Initializes reproduction,
%%%   maybe with mutation.
%%% handle_cast({crossover, Mutation, Partner}, State)
%%%   Interface for the behaviour gen_server. Initializes crossover,
%%%   maybe with mutation.
%%% handle_call(state, From, State)
%%%   Interface for the behaviour gen_server. Returns internal states
%%% handle_cast(stop, State)
%%%   Interface for the behaviour gen_server. Kills the individual
%%% handle_info(Message, State)
%%%   Interface for the behaviour gen_server. Dummy w/o functionality
%%% terminate(Reason, State)
%%%   Interface for the behaviour gen_server. Kills the individual
%%% code_change(OldVersion, State, Extra)
%%%   Interface for the behaviour gen_server. Dummy w/o functionality
%%%---------------------------------------------------------------------

-module(individual).
-author('GEEK1 <erlang@geek1.de>').
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/4, stop/1, state/1, fitness/1]).

-include("records.hrl").

%%----------------------------------------------------------------------
%% Function: start/4
%% Purpose: Calls gen_server:start_link/3 with alphabet, a fitness fun
%%          and the genome
%% Args: Alphabet, the possible alphabet for the genome
%%       and Fitness, the fitness fun
%%       and Genome, the genome of the new individual
%% Returns: {ok,Pid} | ignore | {error,Error}
%%----------------------------------------------------------------------
start(predefined, Alphabet, Fitness, Genome)
	when is_tuple(Alphabet), is_function(Fitness), is_list(Genome) ->
	
	gen_server:start_link(?MODULE, [predefined, Alphabet, Fitness,
		Genome], []);
  
%%----------------------------------------------------------------------
%% Function: start/4
%% Purpose: Calls gen_server:start_link/3 with alphabet, afitness fun
%%           and size to create a random genome
%% Args: Alphabet, the possible alphabet for the genome
%%       and Fitness, the fitness fun
%%       and Size, the size of the new genome
%% Returns: {ok,Pid} | ignore | {error,Error}
%%----------------------------------------------------------------------
start(random, Alphabet, Fitness, Size)
	when is_tuple(Alphabet), is_function(Fitness), is_integer(Size) ->
	
	gen_server:start_link(?MODULE, [random, Alphabet, Fitness, Size], []).
  
%%----------------------------------------------------------------------
%% Function: stop/1
%% Purpose: Kills the individual
%% Args: Pid as pid().
%% Returns: ok.
%%----------------------------------------------------------------------
stop(Pid) when is_pid(Pid) -> gen_server:cast(Pid, stop).

%%----------------------------------------------------------------------
%% Function: state/1
%% Purpose: Returns internal state
%% Args: Pid as pid()
%% Returns: {state, State}
%%----------------------------------------------------------------------
state(Pid) when is_pid(Pid) -> gen_server:call(Pid, state).

%%----------------------------------------------------------------------
%% Function: fitness/1
%% Purpose: Returns the actual fitness
%% Args: Pid as pid()
%% Returns: {state, State}
%%----------------------------------------------------------------------
fitness(Pid) when is_pid(Pid) -> gen_server:call(Pid, fitness).

%%----------------------------------------------------------------------
%% Function: init/1
%% Purpose: Initializes the indivdual with a predefined genome
%% Args: Alphabet, the possible alphabet for the genome
%%       and Fitness, the fitness fun
%%       and Genome, the predefined genome
%% Returns: {ok, State}.
%%----------------------------------------------------------------------
init([predefined, Alphabet, Fitness, Genome])
	when is_tuple(Alphabet), is_function(Fitness), is_list(Genome),
	size(Alphabet) > 0, length(Genome) > 0 ->
	
	{A1,A2,A3} = now(),
	random:seed(A1, A2, A3),
	
	State = #individualState{alphabet = Alphabet, fitness = Fitness,
		genome = Genome},
	
	{ok, State};

%%----------------------------------------------------------------------
%% Function: init/1
%% Purpose: Initializes the indivdual with a unknown genome
%% Args: Alphabet, the possible alphabet for the genome
%%       and Fitness, the fitness fun
%%       and Size, the size of the new genome
%% Returns: {ok, State}.
%%----------------------------------------------------------------------
init([random, Alphabet, Fitness, Size])
	when is_tuple(Alphabet), is_integer(Size), is_function(Fitness),
	size(Alphabet) > 0, Size > 0 ->
	
	{A1,A2,A3} = now(),
	random:seed(A1, A2, A3),
	
	{genome, Genome} = create_genome(Alphabet, Size),
	State = #individualState{alphabet = Alphabet, fitness = Fitness,
		genome = Genome},
	
	{ok, State}.

%%----------------------------------------------------------------------
%% Function: create_genome/2
%% Purpose: creates a new genome
%% Args: Alphabet, the possible alphabet for the genome
%%       and Size, the size of the new genome
%% Returns: {genome, [Genome]}.
%%----------------------------------------------------------------------
create_genome(Alphabet, Size) when is_tuple(Alphabet), Size > 0,
	size(Alphabet) > 0 ->
	
	Gene = element(random:uniform(tuple_size(Alphabet)), Alphabet),
	{genome, Genom} = create_genome(Alphabet, Size - 1),
	
	{genome, [Gene] ++ Genom};

create_genome(_Alphabet, 0) ->
	{genome, []}.

%%----------------------------------------------------------------------
%% Function: mutate_genome/1
%% Purpose: Mutate given genome
%% Args: Genome, the genome which should be mutated
%%       and Alphabet, the possible alphabet
%% Returns: {ok, NewGenome}.
%%----------------------------------------------------------------------
mutate_genome(Genome, Alphabet) ->
	Length = length(Genome),
	Position = random:uniform(Length),
	
	Head = lists:sublist(Genome, Position-1),
	Tail = lists:sublist(Genome, Position+1, Length),
	
	Gene = element(random:uniform(tuple_size(Alphabet)), Alphabet),
	
	NewGenome = Head ++ [Gene] ++ Tail,
	
	{genome, NewGenome}.

%%----------------------------------------------------------------------
%% Function: mixup_genomes/2
%% Purpose: Mixup the given genomes
%% Args: GenomeA, the first genome
%%       and GenomeB, the second genome
%% Returns: {ok, NewGenome}.
%%----------------------------------------------------------------------
mixup_genomes(GenomeA, GenomeB) ->
	Position = round(length(GenomeA)/2),
	
	{Head, _Tail} = lists:split(Position, GenomeA),
	{_Head, Tail} = lists:split(Position, GenomeB),
	
	NewGenome = Head ++ Tail,
	
	{genome, NewGenome}.

%%----------------------------------------------------------------------
%% Function: mutate_childs/1
%% Purpose: Decide if it's time for a mutation and do it
%% Args: MutationRate, the mutation rate
%%       and Genomes, the genomes which could mutate
%%       and state, the actual state
%% Returns: {mutation, Genomes}
%%----------------------------------------------------------------------
mutate_childs(MutationRate, Genomes, State) ->
	if
		MutationRate > 0.0 ->
			Mutation = random:uniform() =< MutationRate;
		true ->
			Mutation = false
	end,

	Position = round(random:uniform(length(Genomes))),
	
	if
		Mutation ->
			Genome = lists:nth(Position, Genomes),
			{genome, MutatedGenome} = mutate_genome(Genome,
				State#individualState.alphabet),
			{mutated, lists:delete(Genome, Genomes) ++ [MutatedGenome]};
		true ->
			{mutated, Genomes}
	end.
	
%%----------------------------------------------------------------------
%% Function: reproduce/1
%% Purpose: Create new individuals based on the given genomes
%% Args: Genomes, the list of genomes which shoudl spwan new individuals
%%       and MutationRate, the mutation rate
%%       and State, the actual state
%% Returns: ok
%%----------------------------------------------------------------------
spawn_childs(Genomes, MutationRate, State) ->
	% mutate if neccesary
	{mutated, NewGenomes} = mutate_childs(MutationRate,
		Genomes, State),
	
	% create childs
	FunMap = fun(Genome) ->
		{ok, Child} = start(predefined, State#individualState.alphabet,
			State#individualState.fitness, Genome),
		{fitness, Fitness} = gen_server:call(Child, fitness),
		
		{Child, Fitness}
	end,
	
	Childs = lists:map(FunMap, NewGenomes),
	
	% casts
	gen_server:cast({global, environment}, {reproduced, self(), Childs}),
	%%%gen_server:cast({global, environment}, {killme, self()}),
	
	ok.

%%----------------------------------------------------------------------
%% Function: handle_call/3
%% Purpose: Returns internal state
%% Args: -
%% Returns: {reply, {state, State}, State}.
%%----------------------------------------------------------------------
handle_call(state, _From, State) -> {reply, {state, State}, State};

%%----------------------------------------------------------------------
%% Function: handle_call/3
%% Purpose: Returns the actual fitness
%% Args: -
%% Returns: {reply, {fitness, Fitness}, State}.
%%----------------------------------------------------------------------
handle_call(fitness, _From, State) ->
	Fun = State#individualState.fitness,
	Fitness = Fun(State),
	
	{reply, {fitness, Fitness}, State};

%%----------------------------------------------------------------------
%% Function: handle_call/3
%% Purpose: Returns the Genome and commits suicide
%% Args: -
%% Returns: {reply, {genome, Genome}, State}.
%%----------------------------------------------------------------------
handle_call(crossover_request, _From, State) ->
	Genome = State#individualState.genome,
	gen_server:cast({global, environment}, {killme, self()}),
	
	{reply, {genome, Genome}, State}.

%%----------------------------------------------------------------------
%% Function: handle_cast/2
%% Purpose: Reproduce itself and report childs to initiator. If mutation
%%          is requested, only one child will mutate.
%% Args: -
%% Returns: {noreply, NewState}.
%%----------------------------------------------------------------------
handle_cast({clone, MutationRate}, State) ->
	Genome = State#individualState.genome,
	
	spawn_childs([Genome, Genome], MutationRate, State),
	
	NewState = State#individualState{
		age = State#individualState.age + 1
	},
	
	{noreply, NewState};

%%----------------------------------------------------------------------
%% Function: handle_cast/2
%% Purpose: Crossover and report the childs to initiator. If mutation is
%%          requested, only one child will mutate.
%% Args: -
%% Returns: {noreply, NewState}.
%%----------------------------------------------------------------------
handle_cast({crossover, MutationRate, Partner}, State) ->
	Genome = State#individualState.genome,
	
	{genome, PartnerGenome} = gen_server:call(Partner, crossover_request),
	
	{genome, GenomeA} = mixup_genomes(Genome, PartnerGenome),
	{genome, GenomeB} = mixup_genomes(PartnerGenome, Genome),
	
	% two childs for each individual
	spawn_childs([GenomeA, GenomeB, GenomeA, GenomeB], MutationRate,
		State),

	NewState = State#individualState{
		age = State#individualState.age + 1
	},
	
	{noreply, NewState};

%%----------------------------------------------------------------------
%% Function: handle_cast/2
%% Purpose: Kills the individual
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
