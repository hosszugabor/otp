%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
%%

-module(ftpd_SUITE).

-include_lib("test_server/include/test_server.hrl").
-include("test_server_line.hrl").

%% Test server specific exports
-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2]).
-export([init_per_testcase/2, end_per_testcase/2,
	 init_per_suite/1, end_per_suite/1]).

-export([start_stop_test/1,
	 connect_test/1,
     	 login_success_test/1,
     	 login_failure_test/1,
	 ls_test/1,
	 ls_dir_test/1,
	 ls_empty_dir_test/1,
	 cd_test/1
	]).

%%--------------------------------------------------------------------
%% all(Arg) -> [Doc] | [Case] | {skip, Comment}
%% Arg - doc | suite
%% Doc - string()
%% Case - atom() 
%%	Name of a test case function. 
%% Comment - string()
%% Description: Returns documentation/test cases in this test suite
%%		or a skip tuple if the platform is not supported.  
%%--------------------------------------------------------------------
suite() -> [].

all() ->
    [start_stop_test,connect_test, {group, login_tests}, ls_test, ls_dir_test, cd_test].

groups() ->
    [{login_tests, [], [login_success_test, login_failure_test]},
     {directory_tests, [], [ls_test, ls_dir_test, ls_empty_dir_test, cd_test]}
    ].

init_per_suite(Config) ->
    ok = inets:start(),
    Config.

end_per_suite(Config) ->
    inets:stop(),
    Config.

init_per_group(login_tests, Config) ->
    {ok, Pid} = inets:start(ftpd, [{port, 2021}, {pwd_fun, fun pwdfun/2}]),
    [{ftpd_pid, Pid} | Config];

init_per_group(directory_tests, Config) ->
    DataDir = ?config(data_dir, Config),
    {ok, Pid} = inets:start(ftpd, [{port, 2021}, {pwd_fun, fun pwdfun/2}, {chrootDir, DataDir}]),
    {ok, Ftp} = ftp:open("localhost", [{port, 2021}]),
    ok = ftp:user(Ftp, "test", "test"),
    [{ftpd_pid, Pid}, {ftp_pid, Ftp} | Config];

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, Config) ->
    case ?config(ftp_pid, Config) of
	undefined ->
	    ok;
	Ftp ->
	    ftp:close(Ftp)
    end,
    case ?config(ftpd_pid, Config) of
	undefined ->
	    ok;
	Pid ->
	    inets:stop(ftpd, Pid)
    end.

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, Config) ->
    Config.

start_stop_test(doc) ->
    ["Test that the FTP server starts at all"];
start_stop_test(suite) ->
    [];
start_stop_test(_Config) ->
    {ok, Pid} = inets:start(ftpd, [{port, 2021}]),
    inets:stop(ftpd, Pid).

connect_test(doc) ->
    ["Test that we can connect to the ftp server"];
connect_test(suite) ->
	[];
connect_test(_Config) ->
    {ok, Pid} = inets:start(ftpd, [{port, 2021}]),
    {ok, Ftp} = ftp:open("localhost", [{port,2021}]),
    ok = ftp:close(Ftp),
    inets:stop(ftpd, Pid).

pwdfun("test", "test") -> authorized;
pwdfun(_, _) -> not_authorized.

login_success_test(doc) ->
    ["Test that a user can login to the FTP server"];
login_success_test(suite) ->
    [];
login_success_test(_Config) ->
    {ok, Ftp} = ftp:open("localhost", [{port, 2021}]),
    ok = ftp:user(Ftp, "test", "test"),
    ftp:close(Ftp).

login_failure_test(doc) ->
    ["Test that a user can not login to the FTP server with wrong password"];
login_failure_test(suite) ->
    [];
login_failure_test(_Config) ->
    {ok, Ftp} = ftp:open("localhost", [{port, 2021}]),
    {error, euser} = ftp:user(Ftp, "test", "test2"),
    ftp:close(Ftp).

ls_test(doc) ->
    ["Test that the user can list the current directory"];
ls_test(suite) ->
    [];
ls_test(Config) ->
    Ftp = ?config(ftp_pid, Config),
    {ok, LsRoot} = ftp:ls(Ftp),
    [Dir, Empty, EmptyDir]=re:split(LsRoot, "\\r\\n", [trim]),
    match = re:run(Dir, "^d.*\sdir$", [{capture, none}]),
    match = re:run(Empty, "^-.*\s0\s+\S+\s+\S+\s+\d+:\d+empty$", [{capture, none}]),
    match = re:run(EmptyDir, "^d.*\sempty_dir$", [{capture, none}]).

ls_dir_test(doc) ->
    ["Test that the user can list a directory"];
ls_dir_test(suite) ->
    [];
ls_dir_test(Config) ->
    Ftp = ?config(ftp_pid, Config),
    {ok, LsDir} = ftp:ls(Ftp, "dir"),
    [OneTwoThree]=re:split(LsDir, "\\r\\n", [trim]),
    match = re:run(OneTwoThree, "^-.*123$", [{capture, none}]).

ls_empty_dir_test(doc) ->
    ["Test that the user can list an empty directory"];
ls_empty_dir_test(suite) ->
    [];
ls_empty_dir_test(Config) ->
    Ftp = ?config(ftp_pid, Config),
    {ok, LsEmpty} = ftp:ls(Ftp, "empty_dir"),
    []=re:split(LsEmpty, "\\r\\n", [trim]).

cd_test(doc) ->
    ["Test that the user can change a directory"];
cd_test(suite) ->
    [];
cd_test(Config) ->
    Ftp = ?config(ftp_pid, Config),
    ok = ftp:cd(Ftp, "dir"),
    {ok, LsDir} = ftp:ls(Ftp),
    [_]=re:split(LsDir, "\\r\\n", [trim]),
    ok = ftp:cd(Ftp, ".."),
    ls_test(Config).

