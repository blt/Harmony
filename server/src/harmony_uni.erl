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
-export([start_link/0, add_star/1, del_star/1]).
-export([add_planet/2, del_planet/2, get_uni/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {bigbang}).

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

%%--------------------------------------------------------------------
%% Function: add_star(Xpos, Ypos) -> {ok, StarId} | {error,Error}
%% Description: Adds a star to the Universe, or fails trying.
%%--------------------------------------------------------------------
add_star(Star=#star{id=null,xpos=Xpos,ypos=Ypos,key=Key})
  when is_integer(Xpos); is_integer(Ypos); is_integer(Key) ->
    gen_server:call(?SERVER, {add_star, Star}, ?TIMEOUT).

%%--------------------------------------------------------------------
%% Function: del_star(StarId) -> -> {ok, StarId} | {error,Error}
%% Description: Deletes a star from the Universe, fails if
%%              star does not exist.
%%--------------------------------------------------------------------
del_star(StarId) when is_integer(StarId) ->
    gen_server:call(?SERVER, {del_star, StarId}, ?TIMEOUT).

%%--------------------------------------------------------------------
%% Function: add_planet(StarId, Angle, Speed, Radius) ->
%%                                         {ok, PlanetId} |
%%                                         {error, Error}
%% Description: Adds a planet to a star in the Universe. Fails
%%              if the star does not exist.
%%--------------------------------------------------------------------
add_planet(StarId, Planet=#planet{id=null,angle=Angle,speed=Speed,
                                  radius=Radius,note=Note})
  when is_integer(StarId); is_integer(Angle); is_integer(Speed);
       is_integer(Radius); is_integer(Note) ->
    harmony_logger:info("Requested addition of planet to star ~p", [StarId]),
    gen_server:call(?SERVER,
                    {add_planet, StarId, Planet},
                    ?TIMEOUT).

%%--------------------------------------------------------------------
%% Function: del_star(StarId, PlanetId) -> {ok, PlanetId}
%%                                       | {error,Error}
%% Description: Deletes a planet from a star in the
%%              Universe. Fails if the star does not exist or
%%              the planet does not exist.
%%--------------------------------------------------------------------
del_planet(StarId, PlanetId)
  when is_integer(StarId), is_integer(PlanetId) ->
    gen_server:call(?SERVER, {del_planet, StarId, PlanetId}, ?TIMEOUT).

get_uni(Time)
  when is_tuple(Time); tuple_size(Time) == 3 ->
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
    {ok, File} = application:get_env(harmony, uni_dets),
    {ok, UniDB} = dets:open_file(uni_db, {file, File}),
    Now = erlang:now(),
    case dets:insert_new(UniDB, {bigbang, Now}) of
        true ->
            State = #state{bigbang=Now};
        false ->
            [{bigbang,BigBang}] = dets:lookup(UniDB, bigbang),
            State = #state{bigbang=BigBang}
    end,
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
handle_call({add_star, InpStar}, _From, State)
  when is_record(InpStar, star) ->
    StarId = star_counter(),
    Star  = InpStar#star{id=StarId},
    Fun = fun() -> mnesia:write(Star) end,
    mnesia:transaction(Fun),
    Reply = {ok, StarId},
    harmony_logger:info("Haved added Star ~p", [Star]),
    {reply, Reply, State};

handle_call({del_star, StarId}, _From, State)
  when is_integer(StarId) ->
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

handle_call({add_planet, StarId, InpPlanet}, _From, State)
  when is_integer(StarId); is_record(InpPlanet, planet) ->
    PlanetId = planet_counter(),
    Now = erlang:now(),
    star_modified(StarId, Now),
    Planet = InpPlanet#planet{id=PlanetId,created=Now},
    harmony_logger:info("Adding planet ~p to star ~p", [PlanetId, StarId]),
    Orbit  = #in_orbit{star_id=StarId, planet_id=PlanetId},
    Fun = fun() ->
                  mnesia:write(Orbit),
                  mnesia:write(Planet)
          end,
    mnesia:transaction(Fun),
    Reply = {ok, PlanetId},
    harmony_logger:info("Haved added Planet ~p", [Planet]),
    {reply, Reply, State};

handle_call({del_planet, StarId, PlanetId}, _From, State)
  when is_integer(StarId); is_integer(PlanetId) ->
    Planet = #planet{id=PlanetId, _='_'},
    Orbit  = #in_orbit{star_id=StarId, planet_id=PlanetId},
    Fun = fun() ->
                  mnesia:delete_object(Orbit),
                  mnesia:delete_object(Planet)
          end,
    mnesia:transaction(Fun),
    Reply = {ok, PlanetId},
    {reply, Reply, State};

handle_call({get_uni, Time}, _From, State)
  when is_tuple(Time); tuple_size(Time) == 3 ->
    F = fun() ->
                Stars = qlc:e(all_stars(Time)),
                Sys = [#system{star=S,
                               planets=qlc:e(all_planets(S, Time))}
                       || S <- Stars],
                #universe{time=erlang:now(), stars=Sys}
        end,
    {atomic, U} = mnesia:transaction(F),
    Reply = {ok, U},
    {reply, Reply, State}.

all_stars(Time)
  when is_tuple(Time); tuple_size(Time) == 3 ->
    qlc:q([S || S <- mnesia:table(star),
                time_ge(S#star.modified,Time)
          ]).

all_planets(Star, Time)
  when is_record(Star, star); is_tuple(Time); tuple_size(Time) == 3 ->
    qlc:q([P ||
              P <- mnesia:table(planet),
              O <- mnesia:table(in_orbit),
              Star#star.id == O#in_orbit.star_id,
              P#planet.id  == O#in_orbit.planet_id,
              time_ge(P#planet.created,Time)
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
handle_info(Info, State) ->
    harmony_logger:info("Universe was cast this message: ~p", [Info]),
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

star_modified(StarID, Now)
  when is_integer(StarID); is_tuple(Now); tuple_size(Now) == 3 ->
    fun() ->
            [S] = mnesia:wread({star, StarID}),
            mnesia:write(S#star{modified=Now})
    end.

planet_counter() ->
    obj_counter(planet).

star_counter() ->
    obj_counter(star).

obj_counter(Type) ->
    mnesia:dirty_update_counter(counter, Type, 1).

map_(F, [H|T]) ->
    F(H),
    map_(F, T);
map_(_F, []) ->
    ok.

time_ge({Msec0, Sec0, Usec0}, {Msec1, Sec1, Usec1}) ->
    T0 = Msec0*1000000 + Sec0 + Usec0/1000000,
    T1 = Msec1*1000000 + Sec1 + Usec1/1000000,
    T0 >= T1.

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------
