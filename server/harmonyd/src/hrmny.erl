%% Copyright (c) 2010 Brian L. Troutwine
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated
%% documentation files (the "Software"), to deal in the
%% Software without restriction, including without limitation
%% the rights to use, copy, modify, merge, publish,
%% distribute, sublicense, and/or sell copies of the Software,
%% and to permit persons to whom the Software is furnished to
%% do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall
%% be included in all copies or substantial portions of the
%% Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY
%% KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
%% WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
%% PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS
%% OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
%% OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
%% OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
%% SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

%%%-------------------------------------------------------------------
%%% File        : hrmny.erl
%%% Author(s)   : Brian L. Troutwine <brian@troutiwne.us>
%%% Description : API and gen_server code for harmonyd
%%% Reference   : gen_server (3)
%%%-------------------------------------------------------------------

-module(hrmny).
-export([start_link/0, start_link/1, stop/0]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2]).
-export([add_star/1, add_planet/2]).
-export([delete_star/1, delete_planet/2]).
-behavior(gen_server).

-include("hrmny.hrl").

%% Behavior required exported functions.

start_link() ->
  start_link("hrmnyDb").

start_link(FileName) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, FileName, []).

stop() ->
  gen_server:cast(?MODULE, stop).

%% Public API

add_star(Xpos, Ypos) ->
    gen_server:call(?MODULE, {add_star, Xpos, Ypos}).
add_planet(StarID, Angle, Radius) ->
    gen_server:call(?MODULE, {add_planet, StarID, Angle, Radius}).

delete_star(StarID) ->
    gen_server:call(?MODULE, {delete_star, StarID}).
delete_planet(PlanetID) ->
    gen_server:call(?MODULE, {delete_planet, PlanetID}).

world_state() ->
    gen_server:call(?MODULE, world_state).

%% Callback Functions

init(FileName) ->
    hrmny_db:create_tables(FileName),
    hrmny_db:restore_backup(),
    {ok, null}.

terminate(_Reason, _LoopData) ->
    hrmny_db:close_tables().

handle_cast(stop, LoopData) ->
    {stop, normal, LoopData}.

handle_call({add_star, Xpos, Ypos}, _From, State) ->
    {reply, Reply, State};

handle_call({add_planet, StarID, Angle, Radius}, _From, State) ->
    {reply, Reply, State};

handle_call({delete_star, StarID}, _From, State) ->
    {reply, Reply, State};

handle_call({delete_planet, PlanetID}, _From, State) ->
    {reply, Reply, State};

handle_call(world_state, _From, State) ->
    {reply, Reply, State}.
