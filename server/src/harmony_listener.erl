%%%-------------------------------------------------------------------
%%% File    : harmony_listener.erl
%%% Author  : Jared T. Sund <jaredsund@gmail.com>
%%% Description :
%%%
%%% Created :  17 Jul 2010 by Jared T. Sund <jaredsund@gmail.com> 
%%%-------------------------------------------------------------------
-module(harmony_listener).

%% API
-export([handler/1]).
-export([listen/0, accept/1]).

-define(TCP_OPTIONS, [binary, {active, false}]).
-define(ListenPort, 1234).

%%error codes
-define(ErrorCode, 0).
-define(CommandFaultCode, 99).
-define(AddStarFaultCode, 1).
-define(DelStarFaultCode, 2).
-define(AddPlanetFaultCode, 4).
-define(DelPlanetFaultCode, 8).
-define(GetUNIFaultCode, 16).


-define(LogFile, 1). %export to logfile = 1, no export !=1 


%%====================================================================
%% API
%%====================================================================

%% %%--------------------------------------------------------------------
%% %% Function: listen().
%% %% Description: open a new tcp socket on the given port
%% %%--------------------------------------------------------------------
listen() ->
    {ok, Socket} = gen_tcp:listen(1234, ?TCP_OPTIONS),
    accept(Socket).

%% %%--------------------------------------------------------------------
%% %% Function: accept(MySocket).
%% %% Description: accepts input from the socket and spawns a new process
%% %%              to handle the data input
%% %%--------------------------------------------------------------------
accept(MySocket) ->
    {ok, Socket} = gen_tcp:accept(MySocket),
    spawn(?MODULE, accept, [MySocket]),
    loop(Socket).

%% %%--------------------------------------------------------------------
%% %% Function: loop(Socket).
%% %% Description: recieve the input from the socket, send it to the data
%% %%              handler, and send a response to the sender
%% %%--------------------------------------------------------------------
loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
	    gen_tcp:send(Socket, handler(Data)),
	    loop(Socket);
	{error, closed} ->
	    ok
    end.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------


%% %%--------------------------------------------------------------------
%% %%--------------------------------------------------------------------
%% %% Harmony Network Protocal
%% %%
%% %% Command:  8 bits {1 byte}, remaining bits are specific to the command
%% %%           being called
%% %%
%% %% :Command
%% %% 1  -> Add Star:      16bits = XPos, 16bits = Yos
%% %%    <- 32 bit StarId, -1 on Error
%% %%
%% %% 2  -> Delete Star:   32bits = StarId
%% %%    <- 32 bit StarId, -1 on Error
%% %%
%% %% 4  -> Add Planet:    32bits = StarId, 16 bits Angle, 16 bits Speed, 16 bits Radius
%% %%    <- 32 bit PlanetId, -1 on Error
%% %%
%% %% 8  -> Delete Planet: 32 bits = StarId, 32 bits = PlanetId
%% %%    <- 32 bit PlanetId, -1 on Error
%% %%
%% %% 16 -> Get Universe: 
%% %%    <-
%% %%--------------------------------------------------------------------
%% %%--------------------------------------------------------------------
%
%% %%--------------------------------------------------------------------
%% %% Function: handler(<<bitString>>) --> {<<response:32, value:32>>}.
%% %% Description: decodes the input bitstring by command, issues the
%% %%              command, builds output bitstring, and returns it
%% %%--------------------------------------------------------------------

handler(<<Command:8, Remaining/bitstring>>) ->
	case Command of
	  1 ->  D = addStar(Remaining);
	  2 ->  D = delStar(Remaining);
	  4 ->  D = addPlanet(Remaining);
	  8 ->  D = delPlanet(Remaining);
	  16 -> D2 = getUNI(Remaining), D ={0,0};%setup for error testing
	  _  -> D = {?ErrorCode, ?CommandFaultCode}
	end,
	outputFile(?LogFile,tuple_to_list(D)), %output to logfile
	buildBitReturn(D); %build the bitString for return to the sender
handler(_T) -> {'Error in input',_T}.

%% %%--------------------------------------------------------------------
%% %% Function: buildBitReturn({...}) --> {"return bitString"}.
%% %% Description: builds a string from the input
%% %%--------------------------------------------------------------------

buildBitReturn({ok, ID}) -> <<1:8,ID:32>>;
%---------------------------------------------
%if we choose to send planet and star IDs back
buildBitReturn({ok, SID, PID}) -> <<1:8,SID:32, PID:32>>;
%---------------------------------------------
buildBitReturn({_,ID}) -> <<?ErrorCode:8,ID:32>>.

%% %%--------------------------------------------------------------------
%% %% Function: addStar(<<XPos:16, YPos:16>>) --> {"return statement"}.
%% %% Description: calls the add star function from the server
%% %%--------------------------------------------------------------------

addStar(<<XPos:16, YPos:16, _/bitstring>>) -> test_server:add_star(XPos, YPos);
addStar(_) -> {?ErrorCode, ?AddStarFaultCode}.

%% %%--------------------------------------------------------------------
%% %% Function: delStar(<<StarId:32>>) --> {"return statement"}.
%% %% Description: calls the del star function from the server
%% %%--------------------------------------------------------------------

delStar(<<StarId:32>>) -> test_server:del_star(StarId);
delStar(_) -> {?ErrorCode, ?DelStarFaultCode}.

%% %%--------------------------------------------------------------------
%% %% Function: addPlanet(<<StarId:32, Angle:16, Speed:16, Radius:16>>) --> 
%% %%    {"return statement"}.
%% %% Description: calls the add Planet function from the server
%% %%--------------------------------------------------------------------

addPlanet(<<StarId:32, Angle:16, Speed:16, Radius:16>>) -> 
	test_server:add_planet(StarId, Angle, Speed, Radius);
addPlanet(_) -> {?ErrorCode, ?AddPlanetFaultCode}.

%% %%--------------------------------------------------------------------
%% %% Function: delPlanet(<<StarId:32, PlanetId:32>>) --> {"return statement"}.
%% %% Description: calls the del Planet function from the server
%% %%--------------------------------------------------------------------

delPlanet(<<StarId:32, PlanetId:32>>)-> 
       test_server:del_planet(StarId, PlanetId);
delPlanet(_) -> {?ErrorCode, ?DelPlanetFaultCode}.

%% %%--------------------------------------------------------------------
%% %% Function: getUNI() --> {"return statement"}.
%% %% Description: Calls the get universe function from the server 
%% %%--------------------------------------------------------------------

getUNI(<<>>) -> test_server:get_uni();
getUNI(_) -> {?ErrorCode, ?GetUNIFaultCode}.

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

outputFile(1,Message) ->
    {ok, Fd} = file:open("log_file",[append]),
    io:fwrite(Fd, "~w~n", [Message]),
    file:close(Fd);
outputFile(_,_) -> nolog.


