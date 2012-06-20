-module(ftpd_listener).

-export([start_link/1]).
-export([new_connection/2,loop/2]).

start_link(Args) ->
	% erlang:register(listener_mod,self()),
	proc_lib:init_ack({ok, self()}),
  %  process_flag(trap_exit, true),
    Port = proplists:get_value(port,Args),
    SupPid = proplists:get_value(sup_pid,Args),
    {ok, LSock} = gen_tcp:listen(Port, [binary, {packet, 0}, 
                                        {active, false}]),
%	loop(LSock,SupPid)
    ListenerPid = spawn_link(ftpd_listener,loop,[LSock, SupPid]),
	l(LSock,ListenerPid)
.

l(LSock,ListenerPid) ->
    receive
		{'EXIT',FromPid,Reason} -> exit(ListenerPid,shutdown), gen_tcp:close(LSock), exit(self(),shutdown)
	end,
	l(LSock,ListenerPid)
.

loop(LSock, SupPid) ->

	{ok, Sock} = gen_tcp:accept(LSock),
	%Args = [Sock],
	spawn(ftpd_listener,new_connection,[Sock, SupPid]),
	loop(LSock, SupPid)
.

new_connection(Sock, SupPid) ->
	erlang:monitor(process, SupPid),
	gen_tcp:send(Sock,"220 abc \r\n"),
	{ok, Data} = do_recv(Sock, []),
	io:format("Data: ~p \n",[Data])
.

do_recv(Sock, Bs) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, B} ->
			io:format("Dataaa: ~p \n",[B]),
            do_recv(Sock, [ B | Bs ]);
        {error, closed} ->
            {ok, list_to_binary(Bs)}
    end.
