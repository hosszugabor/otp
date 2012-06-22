-module(ftpd_listener).

-behaviour(gen_server).

-include_lib("inets/src/inets_app/inets_internal.hrl").

-export([start_link/1, new_connection/3, loop/3]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3, handle_info/2]).


loop(LSock, SupPid, Args) ->
	case gen_tcp:accept(LSock) of
		{ok, Sock} -> spawn_link(ftpd_listener,new_connection,[Sock, SupPid, Args]),
					  loop(LSock, SupPid, Args);
		{_, _Res} -> err_tcp
	end.

new_connection(Sock, SupPid, Args) ->
	erlang:monitor(process, SupPid),
	io:format("---------------- CONNECTION START ----------------\n"),
	send_reply(Sock, 220, "hello"),
	do_recv(Sock, Args).

start_link(Args) ->
	gen_server:start_link({local, ftpd_listener}, ?MODULE, Args, []).

init(Args) ->
	process_flag(trap_exit, true),
	Port = proplists:get_value(port,Args),
    SupPid = proplists:get_value(sup_pid,Args),
    {ok, LSock} = gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}]),
    spawn(?MODULE, loop, [LSock, SupPid, Args]), %% Args also contains SupPid!! TODO
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

%% Denes's code

%%
%% FTP control connection for handling commands
%%

-type proplist()   :: list().
-type reply()      :: {reply, ReplyCode :: integer(), Message :: string()}.
-type argschange() :: sameargs | {newargs, NewArgs :: proplist()}.

%% Wait for incoming messages
do_recv(Sock, Args) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, Data} ->
			DataStr        = binary_to_list(Data),
			{Command, Msg} = packet_to_tokens(DataStr),
			io:format("[Recv]: ~p - ~p\n", [Command, Msg]),

			{Reply, MaybeNewArgs} = handle_message(Command, Msg, Args),
			handle_reply(Sock, Reply),
			NewArgs = case MaybeNewArgs of
			              {newargs, NewArgs1} -> NewArgs1;
			              sameargs            -> Args
			          end,
			after_reply(Sock, Command, NewArgs);
        {error, closed} -> ok
    end.

%% Separate command from message and convert to upper case, eg. "user someone" -> {"USER", ["someone"]}
-spec packet_to_tokens(Data :: string()) -> {Command :: string(), [Message :: string()]}.
packet_to_tokens(Data) ->
	TrimmedData = string:strip(string:strip(Data, right, ?LF), right, ?CR),
	case string:str(TrimmedData, " ") of
		0   -> {string:to_upper(TrimmedData), ""};
		Len ->
			Command = string:to_upper(string:sub_string(TrimmedData, 1, Len - 1)),
			Msg     = string:sub_string(TrimmedData, length(Command) + 2),
    		{Command, string:tokens(Msg, " ")}
	end.

%% Send response if needed
handle_reply(_, noreply) ->
	ok;
handle_reply(Sock, {reply, Code, Message}) ->
	send_reply(Sock, Code, Message).

%% Convert Code and Message to 
send_reply(Sock, Code, Message) ->
	io:format("[Send]: ~p - ~p\n", [Code, Message]),
	Str = integer_to_list(Code) ++ " " ++ Message ++ "\r\n",
	gen_tcp:send(Sock, Str).

%% Construct tuple for response
-spec response(ReplyCode :: integer(), Message :: string()) -> reply().
response(ReplyCode, Message) -> {reply, ReplyCode, Message}.

%% Handle incoming FTP commands
-spec handle_message(Command :: string(), Message :: list(), Args :: proplist()) -> {reply(), argschange()}.
handle_message("NOOP", _, _) ->
	{response(200, "NOOP command successful"), sameargs};

handle_message("QUIT", _, _) ->
	{response(221, "Goodbye."), sameargs};

handle_message("USER", [User|_], Args) ->
  case proplists:get_bool(authed,Args) of
    true -> 
      {response(503, "You are already logged in"), sameargs};
    false ->
      NewArgs1 = proplists:delete(username, Args),
      NewArgs2 = [ {username, User} | NewArgs1],
      {response(331, "Password required for " ++ User), {newargs, NewArgs2}}
  end;

handle_message("PASS", [Password|_], Args) ->
	case {proplists:lookup(username, Args), proplists:get_value(pwd_fun, Args, fun(Uname,Pass) -> not_authorized end)} of
		{{username, User}, PwdFun} ->
			case PwdFun(User, Password) of
				authorized     -> {response(230, "Login ok - TODO: not sure"), {newargs, [authed | Args]}                 };
				not_authorized -> {response(530, "Login incorrect"),           {newargs, proplists:delete(username, Args)}}
			end;
		{none, _} ->
			{response(503, "Login with USER first"), sameargs}
	end;

handle_message("TYPE", Type, Args) ->
	NewArgs = [{repr_type, Type} | Args],
	{response(200, "not yet"), {newargs, NewArgs}};

handle_message("", _, _) ->
	{response(500, "Invalid command: try being more creative"), sameargs};

handle_message(Command, _, _) ->
	{response(500, Command ++ " not implemented"), sameargs}.

%% Control behaviour after replying to a message
after_reply(Sock, "QUIT", _) ->
	io:format("---------------- CONNECTION CLOSE ----------------\n"),
	close_connection(Sock);
after_reply(Sock, _, Args) ->
	do_recv(Sock, Args).

close_connection(Sock) ->
	gen_tcp:close(Sock).
