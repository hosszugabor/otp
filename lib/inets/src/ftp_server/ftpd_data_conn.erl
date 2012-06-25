-module(ftpd_data_conn).

-export([start_passive_mode/0, pasv_accept/1]).

-include_lib("ftpd_rep.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Passive mode
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_passive_mode() ->
	io:format("PASV start\n"), 
	{ok, LSock} = gen_tcp:listen(0, [binary, {packet, 0}, {active, false}]),
	Pid = spawn(?MODULE, pasv_accept, [LSock]),
	{Pid, inet:port(LSock)}.

pasv_accept(LSock) ->
	io:format("PASV accept start\n"), 
	case gen_tcp:accept(LSock) of
		{ok, Sock} -> pasv_send_loop(Sock);
		{_, _Res}  -> err_tcp
	end.

pasv_send_loop(Sock) ->
	io:format("PASV send loop\n"), 
    receive
		{list, Msg, Args} ->
			io:format("PASV send LIST data\n"),
			send_stream(Sock, Msg),
			case Args#ctrl_conn_data.control_socket of
				none -> io:format("PASV control socket lookup fail\n");
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
