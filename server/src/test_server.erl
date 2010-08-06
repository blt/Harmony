-module(test_server).

-export([add_star/3, del_star/1]).
-export([add_planet/5, del_planet/2, get_uni/1]).


add_star(Xpos, Ypos, Key) -> {ok, 32}. %returns StarId
del_star(StarId) ->  {ok, 32}.  %returns StarId
add_planet(StarId, Angle, Speed, Radius, Note) -> {ok, 23}. %returns PlanetId
del_planet(StarId, PlanetId) -> {ok, 23}.  %returns PlanetId


%new uni with notes, keys, and StateId
get_uni(StateId) ->
	{MegSec, Sec, MicroSec} = now(),	
	{ok,
    {universe ,{Sec, MicroSec}
	    ,[
	    {system, {star,1,210,210,9}, [{planet,1,10,5,40,8}, {planet,5,80,10,40,3}, {planet,2,27,20,60,4}]}
	    ,
	    {system, {star,12,210,444410,5}, [{planet,1,10,5,40,12}, {planet,2,27,20,60,4}]}
	    ,
	    {system, {star,3,410,410,2}, [{planet,6,10,5,40,7}, {planet,9,80,10,40,4}, {planet,16,27,20,60,3}]}
	    ,
	    {system, {star,6,310,110,9}, [{planet,6,10,5,40,4}, {planet,9,80,10,40,10}, {planet,16,27,20,60,9}]}
	    ,
	    {system, {star,9,110,510,5}, [{planet,6,10,5,40,2}, {planet,9,80,10,40,2}, {planet,16,27,20,60,4}]}
	    ,
      	    {system, {star,2,610,610,1}, [{planet,3,210,10,30,3}, {planet,4,20,20,50,1}]}
	    ]
    }
}.


