%%%-------------------------------------------------------------------
%%% File    : harmony_server.erl
%%% Author  : Brian L. Troutwine <blt@doritos>
%%% Description :
%%%
%%% Created :  8 Jul 2010 by Brian L. Troutwine <blt@doritos>
%%%-------------------------------------------------------------------
-module(harmony_server).

-behaviour(gen_server).

%% API
-export([start_link/0, add_star/2, del_star/1]).
-export([add_planet/4, del_planet/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(TICKMOD, 10).
-define(TIMEOUT, 150).
-define(SERVER, ?MODULE).
-record(state, {tick=0, utab, objs=0}).
-include("harmony.hrl").

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
    State = #state{},
    {ok, State}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({add_star, Xpos, Ypos}, _From,
            State = #state{objs=StarId}) ->
    Star  = #star{star_id=StarId, xpos=Xpos, ypos=Ypos},
    Fun = fun() -> mnesia:write(Star) end,
    mnesia:transaction(Fun),
    Reply = {ok, StarId},
    {reply, Reply, State#state{objs=StarId+1}};

handle_call({del_star, StarId}, _From, State) ->
    Orbit = #in_orbit{star_id=StarId, planet_id='_'},
    PlanDel = fun(Planet) ->
                      P_id = Planet#planet.planet_id,
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
    Planet = #planet{planet_id=PlanetId, radius=Radius, speed=Speed, angle=Angle},
    Orbit  = #in_orbit{star_id=StarId, planet_id=PlanetId},
    Fun = fun() ->
                  mnesia:write(Orbit),
                  mnesia:write(Planet)
          end,
    mnesia:transaction(Fun),
    Reply = {ok, PlanetId},
    {reply, Reply, State#state{objs=PlanetId+1}};

handle_call({del_planet, StarId, PlanetId}, _From, State) ->
    Planet = #planet{planet_id=PlanetId, radius='_', speed='_', angle='_'},
    Orbit  = #in_orbit{star_id=StarId, planet_id=PlanetId},
    Fun = fun() ->
                  mnesia:delete_object(Orbit),
                  mnesia:delete_object(Planet)
          end,
    mnesia:transaction(Fun),
    Reply = {ok, PlanetId},
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
