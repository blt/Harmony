%%%-------------------------------------------------------------------
%%% File    : harmony_sup.erl
%%% Author  : Brian L. Troutwine <brian@troutwine.us>
%%% Description : Lead supervisor for harmony.
%%%
%%% Created :  7 Jul 2010 by Brian L. Troutwine <brian@troutwine.us>
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

    AChild = {harmony_server, {harmony_server, start_link, []},
              Restart, Shutdown, Type, [harmony_server]},

    {ok, {SupFlags, [AChild]}}.

%%====================================================================
%% Internal functions
%%====================================================================
