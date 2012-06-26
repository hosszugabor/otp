-module(ftpd_data_conn).

-export([start_passive_mode/1, pasv_accept/1]).
-export([reinit_passive_conn/1,send_msg/4]).

-include_lib("ftpd_rep.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Passive mode
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_passive_mode(Ipv) ->
	io:format("PASV start\n"), 
	SockArgs =
		case Ipv of
			inet4 -> [binary, {packet, 0}, {active, false}];
			inet6 -> [binary, {packet, 0}, {active, false}, inet6]
		end,
	{ok, LSock} = gen_tcp:listen(0, SockArgs),
	Pid = spawn(?MODULE, pasv_accept, [LSock]),
	{Pid, inet:port(LSock)}.

reinit_passive_conn(none) ->
	ok;
reinit_passive_conn(LastPid) ->
	exit(LastPid,shutdown).

send_msg(PasvPid,MsgType,Msg,State) ->
	PasvPid ! {MsgType, Msg, State}.

pasv_accept(LSock) ->
	io:format("PASV accept start\n"), 
	case gen_tcp:accept(LSock) of
		{ok, Sock} -> pasv_send_loop(Sock);
		{_, _Res}  -> err_tcp
	end.

pasv_send_loop(Sock) ->
	io:format("PASV send loop\n"), 
    receive
		{list, {FileNames, FullPath}, Args} ->
			io:format("PASV send LIST data\n"),
			TempMsg = [ get_file_info(FName,FullPath) || FName <- FileNames],
			FormattedMsg = string:join(TempMsg, "\r\n") ++ "\r\n",
			send_stream(Sock, FormattedMsg),
			case Args#ctrl_conn_data.control_socket of
				none 		-> io:format("PASV control socket lookup fail\n");
				ControlSock -> send_reply(ControlSock, 226, "Transfer complete")
			end
	end,
	gen_tcp:close(Sock).

%% Convert Code and Message to packet and send
send_reply(Sock, Code, Message) ->
	io:format("[~p-Send]: ~p - ~p\n", [self(), Code, Message]),
	Str = integer_to_list(Code) ++ " " ++ Message ++ "\r\n",
	gen_tcp:send(Sock, Str).

%% Send string without formatting
send_stream(Sock, Msg) ->
	gen_tcp:send(Sock, Msg).

%% Get file information
%% drwxrwsr-x   3 47688    60000        4096 Dec-9-2005 empty

get_file_info(FName,FullPath) ->
%% io:write(FullPath ++ FName),
{ok,{file_info,Size,Type,Access,
%%               {{2012,6,21},{17,20,49}},
			   AccTime,
               ModTime,
               {{CYr,CMn,CDa},{CH,CMin,CSec}},
               Mode,Links,MajorDev,
			MinorDev,INode,UID,GID}} 
		= file:read_file_info(FullPath ++ "/" ++ FName),
	TypeLetter =
	case Type of %% TODO: UNIX types needed
		device -> v;
		directory -> d;
		other -> o;
		regular -> '-';
		symlink -> s;
		_ -> u
	end,
	AccLetter =
	case Access of
		read -> 'r--';
		write -> '-w-';
		read_write -> 'rw-';
		_ -> '---'
	end,
	lists:concat([TypeLetter,AccLetter,AccLetter,AccLetter,
	" ", Links, " ", UID, " ", GID, " ", Size, " ", CDa, " ",
	CMn, " ", CYr, " ", CH, ":", CMin, " ", FName])
.
