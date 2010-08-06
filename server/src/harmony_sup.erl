%%%-------------------------------------------------------------------
%%% File    : harmony_sup.erl
%%% Authors : Brian L. Troutwine <brian@troutwine.us>
%%%           Jared T. Sund      <jaredsund@gmail.com>
%%%           Cameron Kidd       <cameron.kidd@gmail.com>
%%% Description : Lead supervisor for harmony.
%%%
%%% Copyright (c) 2010 Brian L. Troutwine, Jared T. Sund, Cameron Kidd
%%% This code is licensed under the MIT license, see distributed copy.
%%%-------------------------------------------------------------------
-module(harmony_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using
%% supervisor:start_link/[2,3], this function is called by the new process
%% to find out about restart strategy, maximum restart frequency and child
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    Universe = harmony_uni,
    UChild = {Universe, {Universe, start_link, []},
              Restart, Shutdown, Type, [Universe]},

    % get env instead of static set log path and port number
    Logger = harmony_logger,
    LgChild = {Logger, {Logger, start_link, ["/tmp/harmony.log"]},
              Restart, Shutdown, Type, [Logger]},

    Listener = harmony_listener,
    LChild = {Listener, {Listener, start_link, [1234]},
              Restart, Shutdown, Type, [Listener]},

    {ok, {SupFlags, [LgChild, UChild, LChild]}}.

%%====================================================================
%% Internal functions
%%====================================================================
