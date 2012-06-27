-module(ftpd_data_conn).

-export([start_passive_mode/1, start_active_mode/3, pasv_accept/1, actv_accept/1]).
-export([reinit_passive_conn/1, reinit_active_conn/1, send_msg/3]).

-include_lib("ftpd_rep.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Passive mode
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_active_mode(Ipv,Addr,Port) ->
	io:format("PORT start\n"),
	Opts =
		case Ipv of
			inet4 -> [binary, {packet, 0}, {active, false}];
			inet6 -> [binary, {packet, 0}, {active, false}, inet6]
		end,
	case gen_tcp:connect(Addr, Port, Opts) of %% TODO error handling
		{ok, Sock} -> {ok, spawn(?MODULE, actv_accept, [Sock])};
		Error -> Error
	end.

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

reinit_active_conn(LastPid) ->
	reinit_passive_conn(LastPid).

send_msg(MsgType,Msg,State) ->
	case State#ctrl_conn_data.pasv_pid of
		none ->
			{ftpd_ctrl_conn:response(500, "Data connection not established."), sameargs};
		PasvPid ->
			PasvPid ! {MsgType, Msg, State}, %% TODO response msg
			{ftpd_ctrl_conn:response(150, "Opening ASCII mode data connection"), sameargs}
	end.

actv_accept(DataSock) ->
	io:format("ACTV accept start\n"), 
	pasv_send_loop(DataSock).

pasv_accept(LSock) ->
	io:format("PASV accept start\n"), 
	case gen_tcp:accept(LSock) of
		{ok, Sock} -> pasv_send_loop(Sock);
		{_, _Res}  -> err_tcp
	end.

pasv_send_loop(DataSock) ->
	io:format("PASV send loop\n"), 
    receive
		{list, {FileNames, FullPath}, Args} ->
			io:format("PASV send LIST data\n"),
			TempMsg = [ get_file_info(FName,FullPath) || FName <- FileNames],
			FormattedMsg = string:join(TempMsg, "\r\n") ++ "\r\n",
			send_stream(DataSock, FormattedMsg),
			transfer_complete(Args);
		{retr, FileName, Args} ->
			AbsPath = Args#ctrl_conn_data.chrootdir,
			RelPath = Args#ctrl_conn_data.curr_path,
			FPath = AbsPath ++ "/" ++ RelPath ++ "/" ++ FileName,
			case file:read_file(FPath) of
				{ok, Bin} -> send_stream(DataSock, Bin),
							 transfer_complete(Args);
				{error, Reason} ->
					send_ctrl_response(Args, 550, 
								"Requested action not taken. File unavailable, not found, not accessible"),
					io:format("File error: ~p, ~p\n",[Reason,FPath])	
			end;
		{stor, FileName, Args} ->
			AbsPath = Args#ctrl_conn_data.chrootdir, %% TODO duplicated code here and before, solution: make_filepath fun
			RelPath = Args#ctrl_conn_data.curr_path,
			FPath = AbsPath ++ "/" ++ RelPath ++ "/" ++ FileName,
			case receive_and_store(DataSock,FPath) of
				ok 				-> 	transfer_complete(Args); 
				{error, Reason} ->
					send_ctrl_response(Args, 550, 
								"Requested action not taken. File unavailable, not found, not accessible"),
					io:format("File receive error: ~p\n",[Reason])
			end
	end,
	gen_tcp:close(DataSock).


%%	Nothing more just wrapper for gen_tcp:recv
%%	
receive_and_store(DataSock,FPath) ->
	{ok, Id} = file:open(FPath,[append,binary]),
	case {receive_and_write_chunks(DataSock,Id), file:close(Id)} of
		{ok, ok} -> ok;
		_ 		 -> {error, receive_fail}
	end.

receive_and_write_chunks(DataSock,DevId) ->
    case gen_tcp:recv(DataSock, 0) of
        {ok, Data} 		-> file:write(DevId,Data),
						   receive_and_write_chunks(DataSock,DevId);
        {error, closed} -> ok;
		{error, Reason}	-> {error, Reason}
    end.

send_ctrl_response(Args, Command, Msg) ->
	case Args#ctrl_conn_data.control_socket of
		none ->
			ok;
		CtrlSock ->
			send_reply(CtrlSock, Command, Msg)
	end.

transfer_complete(Args) ->
	case Args#ctrl_conn_data.control_socket of
		none 		-> 
			io:format("Data connection failed to look up control connection\n");
		ControlSock -> send_reply(ControlSock, 226, "Transfer complete")
	end.

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
