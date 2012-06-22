-module(ftpd_listener).
-behaviour(gen_server).

-export([start_link/1, new_connection/2, loop/2]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3, handle_info/2]).

loop(LSock, SupPid) ->
	case gen_tcp:accept(LSock) of
		{ok, Sock} -> spawn_link(ftpd_listener,new_connection,[Sock, SupPid]),
					  loop(LSock, SupPid);
		{_, _Res} -> err_tcp
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

start_link(Args) ->
	io:write("abc"),
	gen_server:start_link({local, ftpd_listener}, ?MODULE, Args, []).

init(Args) ->
	process_flag(trap_exit, true),
	Port = proplists:get_value(port,Args),
    SupPid = proplists:get_value(sup_pid,Args),
    {ok, LSock} = gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}]),
    spawn(?MODULE, loop, [LSock, SupPid]),
	{ok, {Port, SupPid, LSock}}.

handle_call(_Req, _From, State) -> {noreply, State}.
handle_cast(_Req, State) -> {noreply, State}.

terminate(shutdown, State) -> 
	LSock = element(3, State),
	gen_tcp:close(LSock),
	io:write("ftpd_listener terminated correctly"),
	ok;

terminate({shutdown, Reason}, State) -> 
	LSock = element(3, State),
	gen_tcp:close(LSock),
	io:write("terminate: 2, ~p/n"),
	ok;

terminate(Reason, State) -> 
	io:write("terminate: 3, ~p/n"),
	LSock = element(3, State),
	gen_tcp:close(LSock),
	ok.

handle_info(Info, State) ->
	io:write("Info: ~p/n"),
	{stop, normal}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

