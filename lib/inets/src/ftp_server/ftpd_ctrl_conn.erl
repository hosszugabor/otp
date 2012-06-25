-module(ftpd_ctrl_conn).

-export([new_connection/3]).

-include_lib("inets/src/inets_app/inets_internal.hrl").

%%
%% FTP control connection for handling commands
%%

-type proplist()   :: proplists:proplist().
-type socket()     :: gen_tcp:socket().

-type reply()      :: {reply, ReplyCode :: integer(), Message :: string()}.
-type argschange() :: sameargs | {newargs, NewArgs :: proplist()}.

-type connitem()   :: ftpd:ftp_option() |
                      {control_socket, Socket :: socket()} |
                      {username, User :: string()} |
                      authed |
                      {repr_type, Params :: list()}.
-type connstate()  :: [connitem()].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Connect and process messages
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new_connection(Sock, SupPid, Args) ->
	erlang:monitor(process, SupPid),
	io:format("---------------- CONNECTION START ----------------\n"),
	send_reply(Sock, 220, "hello"),
	NewArgs = [{control_socket, Sock} | Args],
	do_recv(Sock, NewArgs).

%% Control Connection - Wait for incoming messages
-spec do_recv(Sock :: socket(), Args :: connstate()) -> ok.
do_recv(Sock, Args) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, Data} ->
			DataStr        = binary_to_list(Data),
			{Command, Msg} = packet_to_tokens(DataStr),
			io:format("[~p-Recv]: ~p - ~p\n", [self(), Command, Msg]),
			NewArgs = process_message(Sock, Command, Msg, Args),
			after_reply(Sock, Command, NewArgs);
        {error, closed} -> ok
    end.

process_message(Sock, Command, Msg, Args) ->
	case check_auth(Command, Args) of
		ok ->
			{Reply, MaybeNewArgs} = handle_command(Command, Msg, Args),
			handle_reply(Sock, Reply),
			case MaybeNewArgs of
				{newargs, NewArgs} -> NewArgs;
				sameargs           -> Args
			end;
		bad ->
			handle_reply(Sock, response(530, "Please login with USER and PASS")),
			Args
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Handle incoming FTP commands
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec handle_command(Command :: string(), Message :: list(), Args :: connstate()) -> {reply(), argschange()}.
handle_command("NOOP", _, _) ->
	{response(200, "NOOP command successful"), sameargs};

handle_command("QUIT", _, _) ->
	{response(221, "Goodbye."), sameargs};

handle_command("USER", [User|_], Args) ->
  case proplists:get_bool(authed,Args) of
    true -> 
      {response(503, "You are already logged in"), sameargs};
    false ->
      NewArgs = proplist_modify(username, User, Args),
      {response(331, "Password required for " ++ User), {newargs, NewArgs}}
  end;

handle_command("PASS", [Password|_], Args) ->
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

handle_command("TYPE", Params, Args) ->
	ParamsF = [ string:to_upper(E) || E <- Params],
	case check_repr_type(ParamsF) of
		true ->
			NewArgs = proplist_modify(repr_type, ParamsF, Args),
			{response(200, "TYPE set to " ++ hd(ParamsF)), {newargs, NewArgs}};
		false ->
			{response(500, "'TYPE " ++ string:join(Params, " ") ++ "' not understood"), sameargs}
	end;

handle_command("CWD", Params, Args) ->
	NewDir = string:join(Params, " "),
	CurDir = proplists:get_value(curr_path, Args, "/"),
	case true of
		true ->
			NewArgs = proplist_modify(curr_path, CurDir ++ NewDir, Args),
			{response(250, "CWD command successful."), {newargs, NewArgs}};
		false ->
			{response(550, NewDir ++ ": No such file or directory"), sameargs}
	end;

handle_command("PWD", [], Args) ->
	{response(257, "\"" ++ proplists:get_value(curr_path, Args, "/") ++ "\" is the current directory"), sameargs};

handle_command("PWD", _, _) ->	% TODO: generalize
	{response(501, "Invalid number of arguments"), sameargs};

handle_command("PASV", _, Args) ->
	{ok, Hostname} = inet:gethostname(),
	case inet:getaddr(Hostname, inet) of
		{ok, Address} ->
			{PasvPid, {ok, Port}} = ftpd_data_conn:start_passive_mode(),
			NewArgs = proplist_modify(pasv_pid, PasvPid, Args),
			{response(227, "Entering Passive Mode (" ++ format_address(Address, Port) ++ ")."), {newargs, NewArgs}};
		{error, Error} ->
			io:format("ERROR: inet:getaddr, ~p", [Error]),
			{response(500, "PASV command failed"), sameargs}
	end;

handle_command("PORT", [_Address], _) ->
	{response(500, "TODO"), sameargs};

handle_command("PORT", _, _) ->
	{response(501, "Illegal PORT command"), sameargs};

handle_command("LIST", _, Args) ->
	case proplists:lookup(pasv_pid, Args) of
		{pasv_pid, PasvPid} ->
			PasvPid ! {list, "drwxrwsr-x   3 47688    60000        4096 Dec  9  2005 mirror\r\n", Args},
			{response(150, "Opening ASCII mode data connection for file list"), sameargs};
		none ->
			{response(500, "TODO: LIST fail"), sameargs}
	end;

handle_command("", _, _) ->
	{response(500, "Invalid command: try being more creative"), sameargs};

handle_command(Command, _, _) ->
	{response(500, Command ++ " not implemented"), sameargs}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Control functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Send response if needed
handle_reply(_, noreply) ->
	ok;
handle_reply(Sock, {reply, Code, Message}) ->
	send_reply(Sock, Code, Message).

%% Convert Code and Message to packet and send
send_reply(Sock, Code, Message) ->
	io:format("[~p-Send]: ~p - ~p\n", [self(), Code, Message]),
	Str = integer_to_list(Code) ++ " " ++ Message ++ "\r\n",
	gen_tcp:send(Sock, Str).

%% Send string without formatting
send_stream(Sock, Msg) ->
	gen_tcp:send(Sock, Msg).

%% Construct tuple for response
-spec response(ReplyCode :: integer(), Message :: string()) -> reply().
response(ReplyCode, Message) -> {reply, ReplyCode, Message}.

%% Control behaviour after replying to a message
after_reply(Sock, "QUIT", _) ->
	io:format("---------------- CONNECTION CLOSE ----------------\n"),
	close_connection(Sock);
after_reply(Sock, _, Args) ->
	do_recv(Sock, Args).

close_connection(Sock) ->
	gen_tcp:close(Sock).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Misc functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Converts ip and port to "h1,h2,h3,h4,p1,p2" format
format_address({A1, A2, A3, A4}, Port) ->
	<<P1:8, P2:8>> = <<Port:16>>,
	lists:concat([A1,",",A2,",",A3,",",A4,",",P1,",",P2]).

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

%% check for TYPE command arguments
check_repr_type([Type]) ->
	lists:member(Type, ["A", "E", "I"]);
check_repr_type(["L", Arg]) ->
	Arg == "8";
check_repr_type([_, _]) ->
	true;
check_repr_type(_) ->
	false.

%% Messages that require USER and PASS before
req_auth_messages() -> ["CWD", "PWD", "PASV"].

check_auth(Command, Args) ->
	case {lists:member(Command, req_auth_messages()), proplists:get_bool(authed, Args)} of
		{true, false} -> bad;
		_             -> ok
	end.

