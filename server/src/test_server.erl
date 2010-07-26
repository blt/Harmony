-module(test_server).

-export([add_star/2, del_star/1]).
-export([add_planet/4, del_planet/2, get_uni/0]).


add_star(Xpos, Ypos) -> {ok, 32}. %returns StarId
del_star(StarId) ->  {ok, 32}.  %returns StarId
add_planet(StarId, Angle, Speed, Radius) -> {ok, 23}. %returns PlanetId
del_planet(StarId, PlanetId) -> {ok, 23}.  %returns PlanetId
get_uni() ->{ok,
    {universe
	,now()
	    ,[
	    {system, {star,1,10,10}, [{planet,1,10,10,10}, {planet,2,20,20,20}]}
	    ,
      	    {system, {star,2,10,10}, [{planet,3,10,10,10}, {planet,4,20,20,20}]}
	    ]
    }
}.

get_uni2() ->{ok,
    {universe
	,now()
	    ,[
	    {system, {star,1,10,10}, []}
	    ,
      	    {system, {star,2,10,10}, [{planet,3,10,10,10}, {planet,4,20,20,20}]}
	    ]
    }
}.

get_uni1() ->{ok,{universe ,now(),[]}}.



