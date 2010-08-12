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

-include("harmony.hrl").
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

    mnesia:change_table_copy_type(schema, node(), disc_copies),
    build_table(star, [{attributes, record_info(fields, star)},
                       {disc_copies, [node()]}, {type, set}]),
    build_table(planet, [{attributes,
                          record_info(fields, planet)},
                         {disc_copies, [node()]},
                         {type, set}]),
    build_table(in_orbit, [{attributes, record_info(fields,
                                                    in_orbit)},
                           {disc_copies, [node()]},
                           {type, bag}]),
    build_table(counter, [{attributes, record_info(fields, counter)},
                          {disc_copies, [node()]},
                          {type, set}]),
    mnesia:wait_for_tables([star,planet,in_orbit],5000),

    Universe = harmony_uni,
    UChild = {Universe, {Universe, start_link, []},
              Restart, Shutdown, Type, [Universe]},

    % get env instead of static set log path and port number
    Logger = harmony_logger,
    {ok, Log} = application:get_env(harmony, log_file),
    LgChild = {Logger, {Logger, start_link, [Log]},
              Restart, Shutdown, Type, [Logger]},

    Listener = harmony_listener,
    {ok, Port} = application:get_env(harmony, listen_port),
    LChild = {Listener, {Listener, start_link, [Port]},
              Restart, Shutdown, Type, [Listener]},

    {ok, {SupFlags, [LgChild, UChild, LChild]}}.

%%====================================================================
%% Internal functions
%%====================================================================

% Taken from Vagabond's OpenACD.
% http://github.com/Vagabond/OpenACD
build_table(Tablename, Options) when is_atom(Tablename) ->
    case mnesia:system_info(is_running) =:= yes of
        false ->
            exit(mnesia_stopped);
        true ->ok
    end,
    case mnesia:system_info(use_dir) of
        false ->
            exit(mnesia_schema_not_found);
        true ->
            ok
    end,
    case lists:member(Tablename, mnesia:system_info(local_tables)) of
        true ->
            mnesia:wait_for_tables([Tablename], 5000),
            exists;
        false ->
            case lists:member(Tablename, mnesia:system_info(tables)) of
                true ->
                    mnesia:add_table_copy(Tablename, node(), disc_copies),
                    copied;
                false ->
                    mnesia:create_table(Tablename, Options)
            end
    end.
