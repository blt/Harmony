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
%%% File        : hrmny_sup.erl
%%% Author(s)   : Brian L. Troutwine <brian@troutwine.us>
%%% Description : Supervisor for harmonyd application.
%%% Reference   : supervisor (3)
%%%-------------------------------------------------------------------

-module(hrmny_sup).
-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%% NOTE: We're probably going to need another supervisor for
%%%%       all of the start gen_fsms.

init(FileName) ->
    ChildSpec = {usr,
                 {usr, start_link, []}, % StartFunc
                 permanent,             % Always restart
                 2000,                  % Shutdown timeout (ms?)
                 worker,                % Child process type
                 [usr, usr_db]},        % Replacement modules
    {ok,{
       {one_for_all,                    % Restart all children on failure
        1,1},                           % Die if > 1 failure per second
       [ChildSpec]
      }}.
