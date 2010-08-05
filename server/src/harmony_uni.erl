%%%-------------------------------------------------------------------
%%% File    : harmony_uni.erl
%%% Authors : Brian L. Troutwine <brian@troutwine.us>
%%%           Jared T. Sund      <jaredsund@gmail.com>
%%%           Cameron Kidd       <cameron.kidd@gmail.com>
%%% Description : Persistent core of Harmony.
%%%
%%% Copyright (c) 2010 Brian L. Troutwine, Jared T. Sund, Cameron Kidd
%%% This code is licensed under the MIT license, see distributed copy.
%%%-------------------------------------------------------------------
-module(harmony_uni).
-behaviour(gen_server).

%% API
-export([start_link/0, add_star/2, del_star/1]).
-export([add_planet/4, del_planet/2, get_uni/1]).
-export([load/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(TIMEOUT, 150).
-define(SERVER, ?MODULE).

-record(state, {bigbang, objs=0}).

-include("harmony.hrl").
-include_lib("stdlib/include/qlc.hrl").

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

% Populating a server by hand sucks.
load() ->
    {ok, S0} = add_star(10,10),
    {ok, _P0} = add_planet(S0, 10,10,10),
    {ok, _P1} = add_planet(S0, 10,10,20),
    {ok, S1} = add_star(10,10),
    {ok, _P2} = add_planet(S1, 10,10,10),
    {ok, _P3} = add_planet(S1, 10,10,20).


%%--------------------------------------------------------------------
%% Function: add_star(Xpos, Ypos) -> {ok, StarId} | {error,Error}
%% Description: Adds a star to the Universe, or fails trying.
%%--------------------------------------------------------------------
add_star(Xpos, Ypos) ->
    gen_server:call(?SERVER, {add_star, Xpos, Ypos}, ?TIMEOUT).

%%--------------------------------------------------------------------
%% Function: del_star(StarId) -> -> {ok, StarId} | {error,Error}
%% Description: Deletes a star from the Universe, fails if
%%              star does not exist.
%%--------------------------------------------------------------------
del_star(StarId) ->
    gen_server:call(?SERVER, {del_star, StarId}, ?TIMEOUT).

%%--------------------------------------------------------------------
%% Function: add_planet(StarId, Angle, Speed, Radius) ->
%%                                         {ok, PlanetId} |
%%                                         {error, Error}
%% Description: Adds a planet to a star in the Universe. Fails
%%              if the star does not exist.
%%--------------------------------------------------------------------
add_planet(StarId, Angle, Speed, Radius) ->
    gen_server:call(?SERVER,
                    {add_planet, StarId, Angle, Speed, Radius},
                    ?TIMEOUT).

%%--------------------------------------------------------------------
%% Function: del_star(StarId, PlanetId) -> {ok, PlanetId}
%%                                       | {error,Error}
%% Description: Deletes a planet from a star in the
%%              Universe. Fails if the star does not exist or
%%              the planet does not exist.
%%--------------------------------------------------------------------
del_planet(StarId, PlanetId) ->
    gen_server:call(?SERVER, {del_planet, StarId, PlanetId}, ?TIMEOUT).

get_uni(Time) ->
    gen_server:call(?SERVER, {get_uni, Time}, ?TIMEOUT).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    mnesia:create_table(star,     [{attributes, record_info(fields, star)}]),
    mnesia:create_table(planet,   [{attributes, record_info(fields, planet)}]),
    mnesia:create_table(in_orbit, [{attributes, record_info(fields, in_orbit)},
                                   {type, bag}]),
    mnesia:create_schema([node()|nodes()]),
    mnesia:start(),
    State = #state{bigbang=uninow()},
    {ok, State}.

%%--------------------------------------------------------------------
%% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({add_star, Xpos, Ypos}, _From,
            State = #state{objs=StarId}) ->
    Star  = #star{id=StarId, xpos=Xpos, ypos=Ypos},
    Fun = fun() -> mnesia:write(Star) end,
    mnesia:transaction(Fun),
    Reply = {ok, StarId},
    {reply, Reply, State#state{objs=StarId+1}};

handle_call({del_star, StarId}, _From, State) ->
    Orbit = #in_orbit{star_id=StarId, planet_id='_'},
    PlanDel = fun(Planet) ->
                      P_id = Planet#planet.id,
                      mnesia:delete(planet, P_id)
              end,
    Fun = fun() ->
                  % Find all planets associated with this star.
                  Planets = mnesia:match_object(Orbit),
                  % Delete all associated planets.
                  map_(PlanDel, Planets),
                  % Delete orbit links,
                  mnesia:delete_object(Orbit),
                  % Delete star.
                  mnesia:delete({star, StarId})
          end,
    mnesia:transaction(Fun),
    Reply = {ok, StarId},
    {reply, Reply, State};

handle_call({add_planet, StarId, Angle, Speed, Radius}, _From,
            State = #state{objs=PlanetId}) ->
    Planet = #planet{id=PlanetId, radius=Radius, speed=Speed, angle=Angle},
    Orbit  = #in_orbit{star_id=StarId, planet_id=PlanetId},
    Fun = fun() ->
                  mnesia:write(Orbit),
                  mnesia:write(Planet)
          end,
    mnesia:transaction(Fun),
    Reply = {ok, PlanetId},
    {reply, Reply, State#state{objs=PlanetId+1}};

handle_call({del_planet, StarId, PlanetId}, _From, State) ->
    Planet = #planet{id=PlanetId, _='_'},
    Orbit  = #in_orbit{star_id=StarId, planet_id=PlanetId},
    Fun = fun() ->
                  mnesia:delete_object(Orbit),
                  mnesia:delete_object(Planet)
          end,
    mnesia:transaction(Fun),
    Reply = {ok, PlanetId},
    {reply, Reply, State};

handle_call({get_uni, Time}, _From, State) ->
    F = fun() ->
                Stars = qlc:e(all_stars(Time)),
                Sys = [#system{star=S,
                               planets=qlc:e(all_planets(S, Time))}
                       || S <- Stars],
                #universe{time=uninow(), stars=Sys}
        end,
    {atomic, U} = mnesia:transaction(F),
    Reply = {ok, U},
    {reply, Reply, State}.

all_stars(Time) ->
    qlc:q([S || S <- mnesia:table(star),
                time_ge(S#star.created >= Time)
          ]).

all_planets(Star, Time) ->
    qlc:q([P ||
              P <- mnesia:table(planet),
              O <- mnesia:table(in_orbit),
              Star#star.id == O#in_orbit.star_id,
              P#planet.id  == O#in_orbit.planet_id,
              time_ge(P#planet.created >= Time)
          ]).


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function:

%% %%--------------------------------------------------------------------
%% %% Function: next_id(#state) -> {ID, #state}.
%% %% Description: Provides unique object IDs.
%% %%--------------------------------------------------------------------
%% next_id(State = #state{id_tick=ID}) ->
%%     {ID, #state{id_tick=ID+1}}.

%% %%--------------------------------------------------------------------
%% %% Function: new_star(Xpos, Ypos) -> #star.
%% %% Description: Creates a new star.
%% %%--------------------------------------------------------------------
%% new_star(Xpos, Ypos) ->
%%     #star{xpos=Xpos, ypos=Ypos}.

map_(F, [H|T]) ->
    F(H),
    map_(F, T);
map_(_F, []) ->
    ok.

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------
