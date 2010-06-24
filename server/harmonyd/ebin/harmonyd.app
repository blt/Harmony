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

%% Our music was once divided into its proper forms...It was
%% not permitted to exchange the melodic styles of these
%% established forms and others. Knowledge and informed
%% judgment penalized disobedience. There were no whistles,
%% unmusical mob-noises, or clapping for applause. The rule
%% was to listen silently and learn; boys, teachers, and the
%% crowd were kept in order by threat of the stick. . . . But
%% later, an unmusical anarchy was led by poets who had
%% natural talent, but were ignorant of the laws of
%% music...Through foolishness they deceived themselves into
%% thinking that there was no right or wrong way in music,
%% that it was to be judged good or bad by the pleasure it
%% gave. By their works and their theories they infected the
%% masses with the presumption to think themselves adequate
%% judges. So our theatres, once silent, grew vocal, and
%% aristocracy of music gave way to a pernicious
%% theatrocracy...the criterion was not music, but a
%% reputation for promiscuous cleverness and a spirit of
%% law-breaking.
%%
%% -- Plato, Laws

{application, harmonyd,
 [{description, "Harmony Project Daemon Application"},
  {vsn, "0.0.1"},
  {modules, [usr, usr_db, hrmny_sup, usr_app]},
  {registered, [usr, usr_sup]},
  {applications, [kernel, stdlib]},
  {env, [{dets_name, "usrDb"}]},
  {mod, {usr_app,[]}}]}.
