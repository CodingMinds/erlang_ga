%%%---------------------------------------------------------------------
%%% Description module monitor
%%%---------------------------------------------------------------------
%%% A monitor which cummulate a file with stats abaout the population
%%% and the fitness
%%%---------------------------------------------------------------------
%%% Exports
%%%---------------------------------------------------------------------
%%% start()
%%%   Initializes a new monitor
%%% start(Filename)
%%%   Initializes a new monitor with a specific target filename
%%% state()
%%%   Returns internal states
%%% stop()
%%%   Stops the monitor
%%% init([Filename])
%%%   Interface for the behaviour gen_server.
%%% handle_call(state, From, State)
%%%   Interface for the behaviour gen_server. Returns internal states
%%% handle_cast({population, Age, Population}, State)
%%%   Interface for the behaviour gen_server. Collects data
%%% handle_cast(stop, State)
%%%   Interface for the behaviour gen_server. Stops the monitor
%%% handle_info(Message, State)
%%%   Interface for the behaviour gen_server. Dummy w/o functionality
%%% terminate(Reason, State)
%%%   Interface for the behaviour gen_server. Kills the individual
%%% code_change(OldVersion, State, Extra)
%%%   Interface for the behaviour gen_server. Dummy w/o functionality
%%%---------------------------------------------------------------------

-module(monitor).
-author('GEEK1 <erlang@geek1.de>').
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/0, start/1, state/0, stop/0]).

-include("records.hrl").

%%----------------------------------------------------------------------
%% Function: start/0
%% Purpose: Calls gen_server:start_link/3 with default filename
%% Args: -
%% Returns: {ok,Pid} | ignore | {error,Error}
%%----------------------------------------------------------------------
start() ->
	gen_server:start_link({global, monitor}, ?MODULE, [default_results],
		[]).
  
%%----------------------------------------------------------------------
%% Function: start/1
%% Purpose: Calls gen_server:start_link/3 with the given filename
%% Args: Filename, the target file where all results shoud be stored
%% Returns: {ok,Pid} | ignore | {error,Error}
%%----------------------------------------------------------------------
start(Filename) ->
	gen_server:start_link({global, monitor}, ?MODULE, [Filename], []).
  
%%----------------------------------------------------------------------
%% Function: state/0
%% Purpose: Returns internal state
%% Args: -
%% Returns: {state, State}
%%----------------------------------------------------------------------
state() -> gen_server:call({global, monitor}, state).

%%----------------------------------------------------------------------
%% Function: stop/0
%% Purpose: Stops the monitor
%% Args: -
%% Returns: ok.
%%----------------------------------------------------------------------
stop() -> gen_server:cast({global, monitor}, stop).

%%----------------------------------------------------------------------
%% Function: init/1
%% Purpose: Initializes the monitor with a given filename
%% Args: Filename, the target file where all results should be stored
%% Returns: {ok, State}.
%%----------------------------------------------------------------------
init([Filename]) ->
	
	State = #monitorState{filename = Filename},
	file:delete(Filename),
	
	{ok, State}.

%%----------------------------------------------------------------------
%% Function: handle_call/3
%% Purpose: Returns internal state
%% Args: -
%% Returns: {reply, {state, State}, State}.
%%----------------------------------------------------------------------
handle_call(state, _From, State) -> {reply, {state, State}, State}.

%%----------------------------------------------------------------------
%% Function: handle_cast/2
%% Purpose: Saves the given data
%% Args: -
%% Returns: {noreply, State}.
%%----------------------------------------------------------------------
handle_cast({population, Age, Population}, State) ->
	FunMap = fun(Individual) ->
		{_Pid, Fitness} = Individual,
		Fitness
	end,
	Fitness = lists:sum(lists:map(FunMap, Population)),
	
	AvgFitness = Fitness / length(Population),
	
	file:write_file(State#monitorState.filename,
		io_lib:fwrite("~w\t~w\t~w~n", [Age, Fitness, AvgFitness]), [append]),
	
	{noreply, State};

%%----------------------------------------------------------------------
%% Function: handle_cast/2
%% Purpose: Stops the monitor
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
