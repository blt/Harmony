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
-behavior(gen_server).

%% API
-export([init/1, code_change/3, handle_call/3, handle_cast/2]).
-export([handle_info/2, terminate/2, accept_loop/1]).
-export([start_link/1, addStar/1]).

-define(TCP_OPTIONS, [binary, {active, false}]).

%%error codes
-define(ErrorCode,0).
-define(CommandFault,0).
-define(AddStarFault,1).
-define(DelStarFault,2).
-define(AddPlanetFault,3).
-define(DelPlanetFault,4).
-define(GetUNIFault,5).
-define(SysReturnFault,6).
-define(PlanetReturnFault,7).


%%bit sizes
-define(CommandSize,8).
-define(SuccessSize,8).
-define(IdSize,16).
-define(PositionSize,16).
-define(GenVarSize,16).
-define(CounterSize,16).
-define(KeySize,8).
-define(NoteSize,8).
-define(MegSecSize, 16).
-define(SecSize, 32).
-define(MicSecSize, 32).

-define(SERVER,?MODULE).
-include("harmony.hrl").
-record(server_state, {
          port,
          ip=any,
          lsocket=null}).
-define(UNI, harmony_uni).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% Function: start -> {ok,Pid}
%% Description: Start the listener server.
%%--------------------------------------------------------------------
start_link(Port) ->
    State = #server_state{port=Port},
    gen_server:start_link({local, ?SERVER}, ?MODULE, State, []).

%%--------------------------------------------------------------------
%% Function: init() -> {ok,State}
%% Description: Start the listener server; open a new socket on the given
%% port.
%%--------------------------------------------------------------------
init(State=#server_state{port=Port}) ->
    harmony_logger:info("Harmony Listener online!"),
    process_flag(trap_exit, true),
    case gen_tcp:listen(Port, ?TCP_OPTIONS) of
        {ok, LSocket} ->
            NewState = State#server_state{lsocket = LSocket},
            harmony_logger:info("Listening on socket: ~p",
                                [inet:sockname(LSocket)]),
            {ok, accept(NewState)};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_cast({accepted, _Pid}, State=#server_state{}) ->
    {noreply, accept(State)}.

accept(State = #server_state{lsocket=LSocket}) ->
    harmony_logger:info("Will spawn new accept_loop"),
    Pid = proc_lib:spawn_link(?MODULE, accept_loop, [{self(), LSocket}]),
    harmony_logger:info("New accept_loop has pid: ~p", [Pid]),
    State.

accept_loop({Server, LSocket}) ->
    harmony_logger:info("Listener accept_loop entered."),
    {ok, Socket} = gen_tcp:accept(LSocket),
    harmony_logger:info("Accepted from peer ~p",
                        [inet:peername(Socket)]),
    gen_server:cast(Server, {accepted, self()}),
    loop(Socket).

loop(Socket) ->
    harmony_logger:info("Looping"),
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            Encoded = handler(Data),
            harmony_logger:info("Sending data ~p encoded as ~p.",
                     [Data, Encoded]),
	    gen_tcp:send(Socket, Encoded),
            harmony_logger:info("Data transmition complete."),
	    loop(Socket);
	{error, closed} ->
	    ok
    end.

handle_info({'EXIT',FromPid,Reason},Library) ->
    harmony_logger:info("Pid ~p died with reason ~p", [FromPid,Reason]),
    {noreply, Library};
handle_info(_Msg, Library) -> {noreply, Library}.

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
        _  -> DataReturn = {?ErrorCode, ?CommandFault}
    end,
    %build the bitString for return to the sender
    BitReturn = buildBitReturn(DataReturn),
    BitReturn;
handler(_T) -> {?ErrorCode,_T}.

%%--------------------------------------------------------------------
%% Function: buildBitReturn({...}) --> {"return bitString"}.
%% Description: builds a string from the input
%%--------------------------------------------------------------------

%% Get UNI output format
buildBitReturn({ok,#universe{time={MegSec,Sec,MicroSec},
                             stars=System}}) ->
    NumSys = length(System),
    SystemBits = binlist(<<>>,lists:map(fun sysFull/1, System)),
    <<1:?SuccessSize,MegSec:?MegSecSize,Sec:?SecSize,MicroSec:?MicSecSize,
      NumSys:?CounterSize,SystemBits/bitstring>>;
buildBitReturn({ok, ID}) ->
    <<1:?SuccessSize,ID:?IdSize>>;
buildBitReturn({ok, SID, PID}) ->
    <<1:?SuccessSize,SID:?IdSize, PID:?IdSize>>;
buildBitReturn({0,ID}) ->
    <<?ErrorCode:?SuccessSize,ID:?SuccessSize>>.

%%--------------------------------------------------------------------
%% Function: sysFull
%% Description: Decodes the system data structure to bitstring
%%--------------------------------------------------------------------

sysFull(#system{star=#star{id=StarId,xpos=StarXpos,ypos=StarYpos,
                      key=Key,modified={Meg, Sec, Mic}},
                planets=Planets}) ->
    NumPlanets = length(Planets),
    PlanetBits = binlist(<<>>, lists:map(fun planetFull/1, Planets)),
    <<StarId:?IdSize,StarXpos:?PositionSize,StarYpos:?PositionSize,
      Key:?KeySize,Meg:?MegSecSize,Sec:?SecSize,Mic:?MicSecSize,
	NumPlanets:?CounterSize,PlanetBits/bitstring>>;
sysFull(_) ->
    <<?ErrorCode:?SuccessSize,?SysReturnFault:?SuccessSize>>.

%%--------------------------------------------------------------------
%% Function: planetFull
%% Description: Decodes the planet data structure to bitstring
%%--------------------------------------------------------------------

planetFull(#planet{id=PlanetId,angle=Angle,speed=Speed,
                   radius=Radius,note=Note,
                   created={Meg,Sec,Mic}}) ->
    <<PlanetId:?IdSize,Angle:?GenVarSize,
      Speed:?GenVarSize,Radius:?GenVarSize,Note:?NoteSize
	,Meg:?MegSecSize,Sec:?SecSize,Mic:?MicSecSize>>;
planetFull(_) ->
    <<?ErrorCode:?SuccessSize,?PlanetReturnFault:?SuccessSize>>.

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
addStar(_) -> {?ErrorCode, ?AddStarFault}.

%%--------------------------------------------------------------------
%% Function: delStar(<<StarId>>) --> {"return statement"}.
%% Description: calls the del star function from the server
%%--------------------------------------------------------------------

delStar(<<StarId:?IdSize>>) -> ?UNI:del_star(StarId);
delStar(_) -> {?ErrorCode, ?DelStarFault}.

%%--------------------------------------------------------------------
%% Function: addPlanet(<<StarId, Angle, Speed, Radius, Note>>) -->
%%    {"return statement"}.
%% Description: calls the add Planet function from the server
%%--------------------------------------------------------------------

addPlanet(<<StarId:?IdSize,Angle:?GenVarSize,Speed:?GenVarSize,
            Radius:?GenVarSize,Note:?NoteSize>>) ->
    ?UNI:add_planet(StarId, Angle, Speed, Radius, Note);
addPlanet(_) -> {?ErrorCode, ?AddPlanetFault}.

%%--------------------------------------------------------------------
%% Function: delPlanet(<<StarId, PlanetId>>) --> {"return statement"}.
%% Description: calls the del Planet function from the server
%%--------------------------------------------------------------------

delPlanet(<<StarId:?IdSize, PlanetId:?IdSize>>)->
    ?UNI:del_planet(StarId, PlanetId);
delPlanet(_) -> {?ErrorCode, ?DelPlanetFault}.

%%--------------------------------------------------------------------
%% Function: getUNI() --> {"return statement"}.
%% Description: Calls the get universe function from the server
%%--------------------------------------------------------------------

getUNI(<<MegSec:?MegSecSize, Sec:?SecSize, MicSec:?MicSecSize>>) ->
	?UNI:get_uni({MegSec, Sec, MicSec});
getUNI(_) -> {?ErrorCode, ?GetUNIFault}.

%%--------------------------------------------------------------------
%% Surpress Warnings
%%--------------------------------------------------------------------
handle_call(break, _Caller, State) -> {noreply, State}.
terminate(_Reason, _Library) -> ok.
code_change(_OldVersion, Library, _Extra) -> {ok, Library}.
