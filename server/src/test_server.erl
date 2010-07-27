-module(test_server).

-export([add_star/3, del_star/1]).
-export([add_planet/5, del_planet/2, get_uni/1, location/1]).


add_star(Xpos, Ypos, Key) -> {ok, 32}. %returns StarId
del_star(StarId) ->  {ok, 32}.  %returns StarId
add_planet(StarId, Angle, Speed, Radius, Note) -> {ok, 23}. %returns PlanetId
del_planet(StarId, PlanetId) -> {ok, 23}.  %returns PlanetId

location(StateId) -> {ok, {0, locations, [{1,2,340}, {1,3,215}, {1,5,32}, {2,3,15}, {2,4,359}]}}. 

%new uni with notes, keys, and StateId
get_uni(StateId) ->{ok,
    {universe
	,1
	    ,[
	    {system, {star,1,10,10,5}, [{planet,1,10,10,10,2}, {planet,2,20,20,20,4}]}
	    ,
      	    {system, {star,2,10,10,1}, [{planet,3,10,10,10,3}, {planet,4,20,20,20,1}]}
	    ]
    }
}.

get_uni1(StateId) ->{ok,
    {universe
	,1
	    ,[
	    {system, {star,1,10,10,5}, []}
	    ,
      	    {system, {star,2,10,10,1}, [{planet,3,10,10,10,3}, {planet,4,20,20,20,1}]}
	    ]
    }
}.


get_uni2(StateId) ->{ok, {universe ,1 ,[]}}.

