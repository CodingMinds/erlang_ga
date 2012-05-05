%%%---------------------------------------------------------------------
%%% Description module monitor_trigger
%%%---------------------------------------------------------------------
%%% A monitor which can be used to trigger external scripts
%%%---------------------------------------------------------------------
%%% Exports
%%%---------------------------------------------------------------------
%%% start(Command)
%%%   Initializes a new monitor which executes Command for each received
%%%   dataset.
%%% start(Command, format)
%%%   Initializes a new monitor which executes Command for each received
%%%   dataset.
%%%   The Command can use the modifier of io_lib:format/2, the given
%%%   Data are Age, Fitness and Population, where Population is a list
%%%   of {Pid, Fitness}
%%% start(Command, Divisor)
%%%   Initializes a new monitor which executes Command for each received
%%%   dataset, where the Age is divisible by Divisor.
%%% start(Command, Divisor, format)
%%%   Initializes a new monitor which executes Command for each received
%%%   dataset, where the Age is divisible by Divisor.
%%%   The Command can use the modifier of io_lib:format/2, the given
%%%   Data are Age, Fitness and Population, where Population is a list
%%%   of {Pid, Fitness}
%%% state()
%%%   Returns internal states
%%% stop()
%%%   Stops the monitor
%%% init([Commans, Divisor])
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

-module(monitor_trigger).
-author('GEEK1 <erlang@geek1.de>').
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/1, start/2, start/3, state/0, stop/0]).

-include("records.hrl").

%%----------------------------------------------------------------------
%% Function: start/1
%% Purpose: Calls gen_server:start_link/3 with the given command and the
%%          default divisor
%% Args: Command, the command which should be executed
%% Returns: {ok,Pid} | ignore | {error,Error}
%%----------------------------------------------------------------------
start(Command) ->
	gen_server:start_link({global, monitor}, ?MODULE, [Command, 1, false],
		[]).
  
%%----------------------------------------------------------------------
%% Function: start/2
%% Purpose: Calls gen_server:start_link/3 with the given command and the
%%          default divisor
%% Args: Command, the command which should be executed
%% Returns: {ok,Pid} | ignore | {error,Error}
%%----------------------------------------------------------------------
start(Command, format) ->
	gen_server:start_link({global, monitor}, ?MODULE, [Command, 1, true],
		[]);
  
%%----------------------------------------------------------------------
%% Function: start/2
%% Purpose: Calls gen_server:start_link/3 with the given command and the
%%          given divisor
%% Args: Command, the command which should be executed
%%       and
%% Returns: {ok,Pid} | ignore | {error,Error}
%%----------------------------------------------------------------------
start(Command, Divisor) when is_integer(Divisor) ->
	gen_server:start_link({global, monitor}, ?MODULE, [Command, Divisor,
		false], []).
  
%%----------------------------------------------------------------------
%% Function: start/3
%% Purpose: Calls gen_server:start_link/3 with the given command and the
%%          given divisor
%% Args: Command, the command which should be executed
%%       and
%% Returns: {ok,Pid} | ignore | {error,Error}
%%----------------------------------------------------------------------
start(Command, Divisor, format) when is_integer(Divisor) ->
	gen_server:start_link({global, monitor}, ?MODULE, [Command, Divisor,
		true], []).
  
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
%% Purpose: Initializes the monitor with a given command and divisor
%% Args: Command, the command which should be executed
%%       and Divisor, the divisor which should be used
%%       and Format, a boolean to decide if io_lib:format/2 should be
%%       called
%% Returns: {ok, State}.
%%----------------------------------------------------------------------
init([Command, Divisor, Format]) ->
	State = #monitorState{command = Command, divisor = Divisor,
		call_format = Format},
	
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
%% Purpose: Triggers the Command
%% Args: -
%% Returns: {noreply, State}.
%%----------------------------------------------------------------------
handle_cast({population, Age, Population}, State) ->
	FunMap = fun(Individual) ->
		{_Pid, Fitness} = Individual,
		Fitness
	end,
	
	if
		Age rem State#monitorState.divisor == 0 ->
			Fitness = lists:sum(lists:map(FunMap, Population)),
			
			if
				State#monitorState.call_format ->
					Command = io_lib:format(State#monitorState.command, [Age, Fitness,
						Population]);
				true ->
					Command = State#monitorState.command
			end,
			
			os:cmd(Command);
		true ->
			ok
	end,
	
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
