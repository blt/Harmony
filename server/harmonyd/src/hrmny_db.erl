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
%%% File        : hrmny_db.erl
%%% Author(s)   : Brian L. Troutwine <brian@troutwine.us>
%%% Description : Database API for harmony DB
%%% References  : dets (3), ets (3)
%%%-------------------------------------------------------------------

-module(hrmny_db).
-include("hrmny.hrl").
-export([create_tables/1, restore_backup/0, close_tables/0]).
-export([add_star/0, add_planet/0]).
-export([delete_star/0, delete_planet/0]).
-export([update_star/0, update_planet/0]).

%%%% NOTE: Let's use both DETS and ETS. We'll get persistence,
%%%% and can do fast lookups at the same time.

create_tables(FileName) ->
    {error, not_implemented}.

close_tables() ->
    {error, not_implemented}.

restore_backup() ->
    {error, not_implemented}.

add_star() ->
    {error, not_implemented}.

add_planet() ->
    {error, not_implemented}.

delete_star() ->
    {error, not_implemented}.

delete_planet() ->
    {error, not_implemented}.

update_star() ->
    {error, not_implemented}.

update_planet() ->
    {error, not_implemented}.
