-module(ftpd_listener).

-export([start_link/1]).
-export([new_connection/2,loop/2]).

-include_lib("inets/src/ftp/ftp_internal.hrl").

start_link(Args) ->
	proc_lib:init_ack({ok, self()}),
    Port = proplists:get_value(port,Args),
    SupPid = proplists:get_value(sup_pid,Args),
    {ok, LSock} = gen_tcp:listen(Port, [binary, {packet, 0}, 
                                        {active, false}]),
    spawn_link(ftpd_listener,loop,[LSock, SupPid]),
    mainloop(LSock)
.

mainloop(LSock) ->
    receive
		{'EXIT',Pid,shutdown} -> gen_tcp:close(LSock)
	end,
	mainloop(LSock)
.

loop(LSock, SupPid) ->
	case gen_tcp:accept(LSock) of
		{ok, Sock} -> spawn(ftpd_listener,new_connection,[Sock, SupPid]),
					  loop(LSock, SupPid);
		{_, Res} -> ok
	end
.

new_connection(Sock, SupPid) ->
	erlang:monitor(process, SupPid),
	gen_tcp:send(Sock,"220 abc \r\n"),
	{ok, Data} = do_recv(Sock, []),
	io:format("Data stream end: ~p \n",[Data])
.

do_recv(Sock, Bs) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, B} ->
			io:format("Data recieved: ~p \n",[B]),
            do_recv(Sock, [ B | Bs ]);
        {error, closed} ->
            {ok, list_to_binary(Bs)}
    end.
