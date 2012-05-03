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
%%% handle_cast({reproduce}, State)
%%%   Interface for the behaviour gen_server. Initializes reproduction
%%% handle_cast({mutate}, State)
%%%   Interface for the behaviour gen_server. Initializes reproduction
%%%   with mutation.
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
	{reply, {fitness, Fitness}, State}.

%%----------------------------------------------------------------------
%% Function: handle_cast/2
%% Purpose: Reproduce itself and report childs to initiator
%% Args: -
%% Returns: {noreply, NewState}.
%%----------------------------------------------------------------------
handle_cast(reproduce, State) ->
	{ok, Child1} = start(predefined, State#individualState.alphabet,
		State#individualState.fitness, State#individualState.genome),
	{ok, Child2} = start(predefined, State#individualState.alphabet,
		State#individualState.fitness, State#individualState.genome),
	Childs = [ Child1, Child2 ],
	
	NewState = State#individualState{
		age = State#individualState.age + 1
	},
	
	gen_server:cast({global, environment}, {reproduced, self(), Childs}),
	gen_server:cast(self(), stop),
	
	{noreply, NewState};

%%----------------------------------------------------------------------
%% Function: handle_cast/2
%% Purpose: Reproduce itself and report mutated childs to initiator
%% Args: -
%% Returns: {noreply, NewState}.
%%----------------------------------------------------------------------
handle_cast(mutate, State) ->
	{genome, NewGenome} = mutate_genome(State#individualState.genome,
		State#individualState.alphabet),
	
	{ok, Child1} = start(predefined, State#individualState.alphabet,
		State#individualState.fitness, NewGenome),
	{ok, Child2} = start(predefined, State#individualState.alphabet,
		State#individualState.fitness, NewGenome),
	Childs = [ Child1, Child2 ],
	
	NewState = State#individualState{
		age = State#individualState.age + 1
	},
	
	gen_server:cast({global, environment}, {reproduced, self(), Childs}),
	gen_server:cast(self(), stop),
	
	{noreply, NewState};

%%----------------------------------------------------------------------
%% Function: handle_cast/2
%% Purpose: Kills the individual
%% Args: -
%% Returns: {stop, normal, State}.
%%----------------------------------------------------------------------
handle_cast(stop, State) ->
	gen_server:cast({global, environment}, {dead, self()}),
	
	{stop, normal, State}.

%%----------------------------------------------------------------------
%% Function: *
%% Purpose: Dummy functions for the behaviour gen_server
%%----------------------------------------------------------------------
terminate(normal, _State) -> ok;
terminate(_Reason, _State) -> ok.
handle_info(_Message, State) -> {noreply, State}.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
