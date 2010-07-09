%%%-------------------------------------------------------------------
%%% File    : harmony_universe.erl
%%% Author  : Brian L. Troutwine <blt@doritos>
%%% Description :
%%%
%%% Created :  8 Jul 2010 by Brian L. Troutwine <blt@doritos>
%%%-------------------------------------------------------------------
-module(harmony_universe).

-behaviour(gen_server).

%% API
-export([start_link/0, add_star/2, del_star/1]).
-export([add_planet/4, del_planet/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(TIMEOUT, 150).
-define(SERVER, ?MODULE).
-record(state, {id_tick=0, utab}).
-include("harmony.hrl").

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, #state{}, []).

%%--------------------------------------------------------------------
%% Function: add_star(Xpos, Ypos) -> {ok, StarId} | {error,Error}
%% Description: Adds a star to the Universe, or fails trying.
%%--------------------------------------------------------------------
add_star(Xpos, Ypos) ->
    gen_server:call(huni, {add_star, Xpos, Ypos}, ?TIMEOUT).

%%--------------------------------------------------------------------
%% Function: del_star(StarId) -> -> {ok, StarId} | {error,Error}
%% Description: Deletes a star from the Universe, fails if
%%              star does not exist.
%%--------------------------------------------------------------------
del_star(StarId) ->
    gen_server:call(huni, {del_star, StarId}, ?TIMEOUT).

%%--------------------------------------------------------------------
%% Function: add_planet(StarId, Angle, Speed, Radius) ->
%%                                         {ok, PlanetId} |
%%                                         {error, Error}
%% Description: Adds a planet to a star in the Universe. Fails
%%              if the star does not exist.
%%--------------------------------------------------------------------
add_planet(StarId, Angle, Speed, Radius) ->
    gen_server:call(huni,
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
    gen_server:call(huni, {del_planet, StarId, PlanetId}, ?TIMEOUT).

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
init(State) ->
    NewState = State#state{utab=ets:new(utab, [])},
    erlang:register(huni, self()),
    {ok, NewState}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
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

