-module(ftpd_dir).

-export([set_cwd/3]).

set_cwd(Root, Cwd, Req) ->
	NewReq = dot_correct(Req),
	cwd_fun(Root, Cwd, NewReq).

dot_correct(Cwd) ->
	case string:str(Cwd, "/./") of
		0 -> Cwd;
		Index -> 
			Head = lists:sublist(Cwd, Index-1),
			Tail = lists:nthtail(Index+2, Cwd),
			dot_correct(lists:append([Head, "/", Tail]))
	end.

cwd_fun(_Root, CwdAbsName, "") ->
	{ok, CwdAbsName};

cwd_fun(Root, CwdAbsName, Req) ->
	{Index, Acc, NewReq} = cdd_fun(Req, 0),
	NewCwd = step_back(CwdAbsName, Acc),
	case step_forward(Root, NewCwd, Index, NewReq) of
		{ok, {NewAbsName, NextReq}} -> cwd_fun(Root, NewAbsName, NextReq);
		Error -> Error
	end.
	

step_forward(Root, CwdAbsName, 0, Req) -> 
	NewAbsName = string:join([CwdAbsName, Req], ""),
	case filelib:is_dir(string:join([Root, NewAbsName], "")) of
		true -> {ok, {NewAbsName, ""}};
		false -> {error, invalid_dir}
	end;

step_forward(Root, CwdAbsName, Index, Req) ->
	{CurrPwd, NextReq} = lists:split(Index-1, Req),
	NewAbsName = string:join([CwdAbsName, CurrPwd], ""),
	case filelib:is_dir(string:join([Root, NewAbsName], "")) of
		true -> {ok, {NewAbsName, NextReq}};
		false -> {error, invalid_dir}
	end.

step_back(CwdAbsName, 0) -> 
	CwdAbsName;
step_back("", _) ->
	"";
step_back(CwdAbsName, Acc) ->
	Pos = string:rchr(CwdAbsName, $/),
	NewCwd = lists:sublist(CwdAbsName, Pos-1),
	step_back(NewCwd, Acc-1).


cdd_fun(Req, Acc) ->
	case string:str(Req, "/..") of 
		0 -> {0, Acc, Req};
		1 -> cdd_fun(lists:nthtail(3, Req), Acc+1);
		Index -> {Index, Acc, Req}
	end.
