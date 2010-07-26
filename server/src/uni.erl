-module(uni).

%% API
-export([u/0, h/0]).

u() -> test_server:get_uni().

h() -> uni(test_server:get_uni()).

uni({ok, {universe, {MegSec,Sec,MicroSec}, System}}) ->
	NumSys = length(System),
	SystemBits = sysDecode({<<>>,System}),
	<<1:8, MegSec:32, Sec:32, MicroSec:32, NumSys:16, SystemBits/bitstring>>;
uni(_) -> <<0:8>>.

sysDecode({Out, [{system,{star,StarId,StarXpos,StarYpos} ,Planets}|[]]})-> 
	NumPlanets = length(Planets),
	PlanetBits = planetDecode(<<>>, Planets),
	<<StarId:32, StarXpos:16, StarYpos:16, NumPlanets:16, PlanetBits/bitstring,  Out/bitstring>>;
sysDecode({Out, [{system, {star, StarId, StarXpos, StarYpos}, Planets}|Tail]}) ->
	NumPlanets = length(Planets),
	PlanetBits = planetDecode(<<>>, Planets),
	sysDecode({<<StarId:32, StarXpos:16, StarYpos:16, NumPlanets:16, PlanetBits/bitstring,  Out/bitstring>>,Tail}).

planetDecode(Out, [{planet, PlanetId, Angle, Speed, Radius}|[]]) ->
	<<PlanetId:32, Angle:16, Speed:16, Radius:16, Out/bitstring>>;
planetDecode(Out, [{planet, PlanetId, Angle, Speed, Radius}|Tail]) -> 
	planetDecode(<<PlanetId:32, Angle:16, Speed:16, Radius:16, Out/bitstring>>,Tail).

