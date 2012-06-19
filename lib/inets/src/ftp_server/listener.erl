-module(listener).

-export([start_link/1]).
-export([new_connection/1]).

start_link(Args) ->
	% erlang:register(listener_mod,self()),
	proc_lib:init_ack({ok, self()}),
    Port = Args, %% should be proplist TODO
    {ok, LSock} = gen_tcp:listen(Port, [binary, {packet, 0}, 
                                        {active, false}]),
  %  loop()
    loop(LSock)
.
loop() ->
	loop()
.

loop(LSock) ->
	{ok, Sock} = gen_tcp:accept(LSock),
	Args = [Sock],
	spawn(listener,new_connection,[Args]),
	loop(LSock)
.

new_connection([ Sock | _ ]) ->
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
