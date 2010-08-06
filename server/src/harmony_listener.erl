%%%-------------------------------------------------------------------
%%% File    : harmony_listener.erl
%%% Authors : Jared T. Sund      <jaredsund@gmail.com>
%%%           Brian L. Troutwine <brian@troutwine.us>
%%%           Cameron Kidd       <cameron.kidd@gmail.com>
%%% Description : TCP interface to Harmony's persistent core.
%%%
%%% Copyright (c) 2010 Jared T. Sund, Brian L. Troutwine, Cameron Kidd
%%% This code is licensed under the MIT license, see distributed copy.
%%%-------------------------------------------------------------------
-module(harmony_listener).

%% API
-export([start_link/0, init/1]).
-export([handler/1]).

-define(ListenPort,1234).

%%export to logfile = 1, no export !=1
-define(LogFile,1).

%%error codes
-define(ErrorCode,0).
-define(CommandFaultCode,99).
-define(AddStarFaultCode,1).
-define(DelStarFaultCode,2).
-define(AddPlanetFaultCode,4).
-define(DelPlanetFaultCode,8).
-define(GetUNIFaultCode,16).

%%bit sizes
-define(CommandSize,8).
-define(SuccessSize,8).
-define(TimeSize,64).
-define(IdSize,16).
-define(PositionSize,16).
-define(GenVarSize,16).
-define(CounterSize,16).
-define(KeySize,8).
-define(NoteSize,8).

-define(SERVER,?MODULE).
-define(UNI, harmony_uni).

%%====================================================================
%% API
%%====================================================================

%% OTP Special Process Callbacks, not quite complete.

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid}
%% Description: Start the listener server.
%%--------------------------------------------------------------------
start_link() ->
    Pid = proc_lib:start_link({local, ?SERVER}, ?MODULE, init, [self()]),
    {ok,Pid}.

%%--------------------------------------------------------------------
%% Function: init() -> {ok,State}
%% Description: Start the listener server.
%%--------------------------------------------------------------------
init(Parent) ->
    register(?MODULE, self()),
    proc_lib:init_ack(Parent, {ok, self()}),
    listen().


%%--------------------------------------------------------------------
%% Function: listen().
%% Description: open a new tcp socket on the given port
%%--------------------------------------------------------------------
listen() ->
    {ok, Socket} = gen_tcp:listen(1234, [binary, {active, false}]),
    accept(Socket).

%%--------------------------------------------------------------------
%% Function: accept(MySocket).
%% Description: accepts input from the socket and spawns a new process
%%              to handle the data input
%%--------------------------------------------------------------------
accept(MySocket) ->
    {ok, Socket} = gen_tcp:accept(MySocket),
    spawn(?MODULE, fun() -> accept(MySocket) end),
    loop(Socket).

%%--------------------------------------------------------------------
%% Function: loop(Socket).
%% Description: recieve the input from the socket, send it to the data
%%              handler, and send a response to the sender
%%--------------------------------------------------------------------
loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            outputFile(?LogFile, ['---log entry', date(), time()]),
	    gen_tcp:send(Socket, handler(Data)),
            outputFile(?LogFile, tuple_to_list(inet:peername(Socket))),
	    loop(Socket);
	{error, closed} ->
	    ok
    end.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------


%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% Harmony Network Protocal
%%
%% Command:  8 bits {1 byte}, remaining bits are specific to the command
%%           being called
%%
%% :Command
%% 1  -> Add Star:      XPos, Yos
%%    <- StarId, 0 on Error
%%
%% 2  -> Delete Star:   StarId
%%    <- StarId, 0 on Error
%%
%% 4  -> Add Planet:    StarId, Angle, Speed, Radius
%%    <- PlanetId, 0 on Error
%%
%% 8  -> Delete Planet: StarId, PlanetId
%%    <- PlanetId, 0 on Error
%%
%% 16 -> Get Universe:
%%    <- universe output
%%	{ok,{universe,{megsec,sec,microsec},
%%		[{system,{star,id,x,y},
%%			[{planet,id,angle,speed,radius}, ... ,{planet,id,angle,speed,radius}]},
%%		.....,
%%		{system,{star,id,x,y},
%%			[{planet,id,angle,speed,radius}, ... ,{planet,id,angle,speed,radius}]}]}}
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%
%%--------------------------------------------------------------------
%% Function: handler(<<bitString>>) --> {<<response, value>>}.
%% Description: decodes the input bitstring by command, issues the
%%              command, builds output bitstring, and returns it
%%--------------------------------------------------------------------

handler(<<Command:?CommandSize, Remaining/bitstring>>) ->
    case Command of
        1 ->  DataReturn = addStar(Remaining);
        2 ->  DataReturn = delStar(Remaining);
        4 ->  DataReturn = addPlanet(Remaining);
        8 ->  DataReturn = delPlanet(Remaining);
        16 -> DataReturn = getUNI(Remaining);
        _  -> DataReturn = {?ErrorCode, ?CommandFaultCode}
    end,
    outputFile(?LogFile,tuple_to_list(DataReturn)), %output to logfile
    %build the bitString for return to the sender
    BitReturn = buildBitReturn(DataReturn),
    outputFile(?LogFile, binary_to_list(BitReturn)),
    BitReturn;
handler(_T) -> {'Error in input',_T}.

%%--------------------------------------------------------------------
%% Function: buildBitReturn({...}) --> {"return bitString"}.
%% Description: builds a string from the input
%%--------------------------------------------------------------------

%% Get UNI output format
%% [0/1], megSec, sec, micro, #stars, starid, x, y, key
%%       #planets, planetid, angle, speed, radius,note
buildBitReturn({ok, {universe, {Sec, MicroSec}, System}}) ->
    NumSys = length(System),
    SystemBits = binlist(<<>>,lists:map(fun sysFull/1, System)),
    <<1:?SuccessSize,Sec:?GenVarSize,MicroSec:?GenVarSize,
      NumSys:?CounterSize,SystemBits/bitstring>>;

%% location output format
%% [0/1], startId, planetId, angle
buildBitReturn({ok, {State, locations, PlanetList}}) ->
    NumPlanets = length(PlanetList),
    PlanetBits = binlist(<<>>, lists:map(fun planetLocation/1, PlanetList)),
    <<State:?SuccessSize, NumPlanets:?CounterSize, PlanetBits/bitstring>>;

buildBitReturn({ok, ID}) ->
    <<1:?SuccessSize,ID:?IdSize>>;
buildBitReturn({ok, SID, PID}) ->
    <<1:?SuccessSize,SID:?IdSize, PID:?IdSize>>;
buildBitReturn({_,ID}) ->
    <<?ErrorCode:?SuccessSize,ID:?IdSize>>.

%%--------------------------------------------------------------------
%% Function: sysFull
%% Description: Decodes the system data structure to bitstring
%%--------------------------------------------------------------------

sysFull({system, {star, StarId, StarXpos, StarYpos, Key}, Planets}) ->
    NumPlanets = length(Planets),
    PlanetBits = binlist(<<>>, lists:map(fun planetFull/1, Planets)),
    <<StarId:?IdSize,StarXpos:?PositionSize,StarYpos:?PositionSize,
      Key:?KeySize,NumPlanets:?CounterSize,PlanetBits/bitstring>>.

%%--------------------------------------------------------------------
%% Function: planetFull
%% Description: Decodes the planet data structure to bitstring
%%--------------------------------------------------------------------

planetFull({planet, PlanetId, Angle, Speed, Radius, Note}) ->
    <<PlanetId:?IdSize,Angle:?GenVarSize,
      Speed:?GenVarSize,Radius:?GenVarSize,Note:?NoteSize>>.

%%--------------------------------------------------------------------
%% Function: planetLocation
%% Description: Decodes the planet data location structure to bitstring
%%--------------------------------------------------------------------

planetLocation({StarId, PlanetId, Angle})->
    <<StarId:?IdSize, PlanetId:?IdSize, Angle:?GenVarSize>>.

%%--------------------------------------------------------------------
%% Function: binlist
%% Description: turns a list of bitstrings into a single bitstring
%%--------------------------------------------------------------------

binlist(Out, []) -> <<Out/bitstring>>;
binlist(Out, [Head | []]) ->  <<Head/bitstring, Out/bitstring>>;
binlist(Out, [Head | Tail]) ->
    binlist(<<Head/bitstring, Out/bitstring>>, Tail).

%%--------------------------------------------------------------------
%% Function: addStar(<<XPos, YPos, Key>>) --> {"return statement"}.
%% Description: calls the add star function from the server
%%--------------------------------------------------------------------

addStar(<<XPos:?PositionSize, YPos:?PositionSize, Key:?KeySize>>) ->
    ?UNI:add_star(XPos, YPos, Key);
addStar(_) -> {?ErrorCode, ?AddStarFaultCode}.

%%--------------------------------------------------------------------
%% Function: delStar(<<StarId>>) --> {"return statement"}.
%% Description: calls the del star function from the server
%%--------------------------------------------------------------------

delStar(<<StarId:?IdSize>>) -> ?UNI:del_star(StarId);
delStar(_) -> {?ErrorCode, ?DelStarFaultCode}.

%%--------------------------------------------------------------------
%% Function: addPlanet(<<StarId, Angle, Speed, Radius, Note>>) -->
%%    {"return statement"}.
%% Description: calls the add Planet function from the server
%%--------------------------------------------------------------------

addPlanet(<<StarId:?IdSize,Angle:?GenVarSize,Speed:?GenVarSize,
            Radius:?GenVarSize,Note:?NoteSize>>) ->
    ?UNI:add_planet(StarId, Angle, Speed, Radius, Note);
addPlanet(_) -> {?ErrorCode, ?AddPlanetFaultCode}.

%%--------------------------------------------------------------------
%% Function: delPlanet(<<StarId, PlanetId>>) --> {"return statement"}.
%% Description: calls the del Planet function from the server
%%--------------------------------------------------------------------

delPlanet(<<StarId:?IdSize, PlanetId:?IdSize>>)->
    ?UNI:del_planet(StarId, PlanetId);
delPlanet(_) -> {?ErrorCode, ?DelPlanetFaultCode}.

%%--------------------------------------------------------------------
%% Function: getUNI() --> {"return statement"}.
%% Description: Calls the get universe function from the server
%%--------------------------------------------------------------------

getUNI(<<StateId:?GenVarSize>>) -> ?UNI:get_uni(StateId);
getUNI(_) -> {?ErrorCode, ?GetUNIFaultCode}.

%%--------------------------------------------------------------------
%% Logging
%%--------------------------------------------------------------------

outputFile(1,Message) ->
    {ok, Fd} = file:open("log_file",[append]),
    io:fwrite(Fd, "~w~n", [Message]),
    file:close(Fd);
outputFile(_,_) -> nolog.
