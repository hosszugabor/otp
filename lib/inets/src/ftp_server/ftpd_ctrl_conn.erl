-module(ftpd_ctrl_conn).

-export([new_connection/3]).

-include_lib("inets/src/inets_app/inets_internal.hrl").

-include_lib("ftpd_rep.hrl").

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
	send_reply(Sock, 220, "Hello"),
	ErlTop = element(2,file:get_cwd()),
	ChRootDir = proplists:get_value(chrootDir, Args, ErlTop), %% HINT chrootDir vs chrootdir
	PwdFun = proplists:get_value(pwd_fun, Args, fun(_U,_P) -> not_authorized end), 
	ConnData = #ctrl_conn_data{ control_socket 	= Sock, 
								pwd_fun			= PwdFun,
								chrootdir		= ChRootDir	 },
	do_recv(Sock, ConnData).

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

handle_command("USER", [User|_], Args ) ->
  case Args#ctrl_conn_data.authed of
    true -> 
      {response(503, "You are already logged in"), sameargs};
    false ->
      NewArgs = Args#ctrl_conn_data{ username = User },
      {response(331, "Password required for " ++ User), {newargs, NewArgs}}
  end;

handle_command("PASS", [Password|_], Args) ->
	Authed = Args#ctrl_conn_data.authed,
	User   = Args#ctrl_conn_data.username,
	case {Authed, User} of
		{false, none} -> {response(503, "Login with USER first"),     sameargs};
		{false, _} ->
			PwdFun = Args#ctrl_conn_data.pwd_fun,
			case PwdFun(User, Password) of
				authorized     -> {response(230, "Login ok - TODO: not sure"), {newargs, Args#ctrl_conn_data{ authed = true }}};
				not_authorized -> {response(530, "Login incorrect"),           {newargs, Args#ctrl_conn_data{ username = none }}}
			end;
		{true, _}     -> {response(503, "You are already logged in"), sameargs}
	end;

handle_command("TYPE", Params, Args) ->
	ParamsF = [ string:to_upper(E) || E <- Params],
	case check_repr_type(ParamsF) of
		true ->
			NewArgs = Args#ctrl_conn_data{ repr_type = ParamsF },
			{response(200, "TYPE set to " ++ hd(ParamsF)), {newargs, NewArgs}};
		false ->
			{response(500, "'TYPE " ++ string:join(Params, " ") ++ "' not understood"), sameargs}
	end;

handle_command("CWD", Params, Args) ->
	NewDir = string:join(Params, " "),
	CurDir = Args#ctrl_conn_data.curr_path,
	BaseDir = Args#ctrl_conn_data.chrootdir,
	case ftpd_dir:set_cwd(BaseDir, CurDir, NewDir) of
		{ok, NewPath} ->
			io:format("CWD new path: ~p", [NewPath]),
			NewArgs = Args#ctrl_conn_data{ curr_path = NewPath },
			{response(250, "CWD command successful."), {newargs, NewArgs}};
		{error, Error} ->
			io:format("CWD error: ~p", [Error]),
			{response(550, NewDir ++ ": No such file or directory"), sameargs}
	end;

handle_command("PWD", [], Args) ->
	{response(257, "\"" ++ Args#ctrl_conn_data.curr_path ++ "\" is the current directory"), sameargs};

handle_command("PWD", _, _) ->	% TODO: generalize
	{response(501, "Invalid number of arguments"), sameargs};

handle_command("PASV", _, Args) ->
	{ok, Hostname} = inet:gethostname(),
	case inet:getaddr(Hostname, inet) of
		{ok, Address} ->
			ftpd_data_conn:reinit_passive_conn(Args#ctrl_conn_data.pasv_pid),
			{PasvPid, {ok, Port}} = ftpd_data_conn:start_passive_mode(),
			NewArgs = Args#ctrl_conn_data{ pasv_pid = PasvPid },
			{response(227, "Entering Passive Mode (" ++ format_address(Address, Port) ++ ")."), {newargs, NewArgs}};
		{error, Error} ->
			io:format("ERROR: inet:getaddr, ~p", [Error]),
			{response(500, "PASV command failed"), sameargs}
	end;

handle_command("PORT", [_Address], _) ->
	{response(500, "TODO"), sameargs};

handle_command("PORT", _, _) ->
	{response(501, "Illegal PORT command"), sameargs};

handle_command("LIST", Params, Args) ->
	case Args#ctrl_conn_data.pasv_pid of
		none ->
			{response(500, "TODO: LIST fail"), sameargs};
		PasvPid ->
			DirToList = string:join(Params, " "), %% TODO
			AbsPath = Args#ctrl_conn_data.chrootdir,
			RelPath = Args#ctrl_conn_data.curr_path,
			case ftpd_dir:set_cwd(AbsPath, RelPath, DirToList) of
				{ok, NewPath} ->
					FullPath = AbsPath ++ "/" ++ RelPath ++ DirToList,
					io:format("LIST path: ~p", [FullPath]),
				%	io:write(AbsPath),
					io:write(NewPath),
					{ok, FileNames} = file:list_dir(AbsPath ++ NewPath),
					ftpd_data_conn:send_msg(PasvPid,list, {FileNames, FullPath},
																		 Args),
					{response(150, "Opening ASCII mode data connection for file list"), sameargs};
				{error, Error} ->
					io:format("LIST error: ~p  | ~p | ~p | ~p | ~p", [Error,Params,DirToList,AbsPath,RelPath]),
					{response(550, "LIST fail TEMP TODO"), sameargs}
			end
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
	case {lists:member(Command, req_auth_messages()), Args#ctrl_conn_data.authed } of
		{true, false} -> bad;
		_             -> ok
	end.

