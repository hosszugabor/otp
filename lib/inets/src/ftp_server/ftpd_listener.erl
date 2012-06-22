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

%terminate(shutdown, State) removed, same body
%terminate({shutdown, _Reason}, State)
terminate(_Reason, State) -> 
	io:write("terminate: 3/n"),
	LSock = element(3, State),
	gen_tcp:close(LSock),
	ok.

handle_info(_Info, _State) ->
	{stop, normal}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% Denes's code

%%
%% FTP control connection for handling commands
%%

-type proplist()   :: proplists:proplist().
-type reply()      :: {reply, ReplyCode :: integer(), Message :: string()}.
-type argschange() :: sameargs | {newargs, NewArgs :: proplist()}.
-type connitem()   :: ftpd:ftp_option() |
                      {username, User :: string()} |
                      authed |
                      {repr_type, Params :: list()}.
-type connstate()  :: [connitem()].
-type socket()     :: gen_tcp:socket().

%% Wait for incoming messages
-spec do_recv(Sock :: socket(), Args :: connstate()) -> ok.
do_recv(Sock, Args) ->
	io:format("sockname ~p\n", [inet:sockname(Sock)]),
	io:format("peername ~p\n", [inet:peername(Sock)]),
    case gen_tcp:recv(Sock, 0) of
        {ok, Data} ->
			DataStr        = binary_to_list(Data),
			{Command, Msg} = packet_to_tokens(DataStr),
			io:format("[~p-Recv]: ~p - ~p\n", [self(), Command, Msg]),

			{Reply, MaybeNewArgs} = handle_message(Command, Msg, Args),
			handle_reply(Sock, Reply),
			NewArgs = case MaybeNewArgs of
			              {newargs, NewArgs1} -> NewArgs1;
			              sameargs            -> Args
			          end,
			after_reply(Sock, Command, NewArgs);
        {error, closed} -> ok
    end.

%% Send response if needed
handle_reply(_, noreply) ->
	ok;
handle_reply(Sock, {reply, Code, Message}) ->
	send_reply(Sock, Code, Message).

%% Convert Code and Message to 
send_reply(Sock, Code, Message) ->
	io:format("[~p-Send]: ~p - ~p\n", [self(), Code, Message]),
	Str = integer_to_list(Code) ++ " " ++ Message ++ "\r\n",
	gen_tcp:send(Sock, Str).

%% Construct tuple for response
-spec response(ReplyCode :: integer(), Message :: string()) -> reply().
response(ReplyCode, Message) -> {reply, ReplyCode, Message}.

%% Handle incoming FTP commands
-spec handle_message(Command :: string(), Message :: list(), Args :: connstate()) -> {reply(), argschange()}.
handle_message("NOOP", _, _) ->
	{response(200, "NOOP command successful"), sameargs};

handle_message("QUIT", _, _) ->
	{response(221, "Goodbye."), sameargs};

handle_message("USER", [User|_], Args) ->
  case proplists:get_bool(authed,Args) of
    true -> 
      {response(503, "You are already logged in"), sameargs};
    false ->
      NewArgs = proplist_modify(username, User, Args),
      {response(331, "Password required for " ++ User), {newargs, NewArgs}}
  end;

handle_message("PASS", [Password|_], Args) ->
	Authed = proplists:get_bool(authed, Args),
	User   = proplists:lookup(username, Args),
	case {Authed, User} of
		{false, {username, UserName}} ->
			PwdFun = proplists:get_value(pwd_fun, Args, fun(_U,_P) -> not_authorized end),
			case PwdFun(UserName, Password) of
				authorized     -> {response(230, "Login ok - TODO: not sure"), {newargs, [authed | Args]}                 };
				not_authorized -> {response(530, "Login incorrect"),           {newargs, proplists:delete(username, Args)}}
			end;
		{false, none} -> {response(503, "Login with USER first"),     sameargs};
		{true, _}     -> {response(503, "You are already logged in"), sameargs}
	end;

handle_message("TYPE", Params, Args) ->
	ParamsF = [ string:to_upper(E) || E <- Params],
	case check_repr_type(ParamsF) of
		true ->
			NewArgs = proplist_modify(repr_type, ParamsF, Args),
			{response(200, "TYPE set to " ++ hd(ParamsF)), {newargs, NewArgs}};
		false ->
			{response(500, "'TYPE " ++ string:join(Params, " ") ++ "' not understood"), sameargs}
	end;

handle_message("CWD", Params, Args) ->
	NewDir = string:join(Params, " "),
	CurDir = proplists:get_value(cwd, Args, "/"),
	case true of
		true ->
			NewArgs = proplist_modify(cwd, CurDir ++ NewDir, Args),
			{response(250, "CWD command successful."), {newargs, NewArgs}};
		false ->
			{response(550, NewDir ++ ": No such file or directory"), sameargs}
	end;

handle_message("PWD", [], Args) ->
	{response(257, "\"" ++ proplists:get_value(cwd, Args, "/") ++ "\" is the current directory"), sameargs};

handle_message("PWD", _, _) ->	% TODO: generalize
	{response(501, "Invalid number of arguments"), sameargs};

handle_message("PASV", _, _) ->
	{response(500, "TODO"), sameargs};

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

%% MISC functions

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

proplist_modify(Key, Value, PropList) ->
	[{Key, Value} | proplists:delete(Key, PropList)].

%%
check_repr_type([Type]) ->
	lists:member(Type, ["A", "E", "I"]);
check_repr_type(["L", Arg]) ->
	Arg == "8";
check_repr_type([_, _]) ->
	true;
check_repr_type(_) ->
	false.
