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
     	 login_failure_test/1
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
suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [start_stop_test,connect_test, login_success_test, login_failure_test].

groups() ->
    [].

init_per_suite(Config) ->
    ok = inets:start(),
    Config.

end_per_suite(Config) ->
    inets:stop(),
    Config.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, Config) ->
    Config.

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
    {ok, Pid} = inets:start(ftpd, [{port, 2021}, {pwd_fun, fun pwdfun/2}]),
    {ok, Ftp} = ftp:open("localhost", [{port, 2021}]),
    ok = ftp:user(Ftp, "test", "test"),
    ftp:close(Ftp),
    inets:stop(ftpd, Pid).

login_failure_test(doc) ->
    ["Test that a user can not login to the FTP server with wrong password"];
login_failure_test(suite) ->
    [];
login_failure_test(_Config) ->
    {ok, Pid} = inets:start(ftpd, [{port, 2021}, {pwd_fun, fun pwdfun/2}]),
    {ok, Ftp} = ftp:open("localhost", [{port, 2021}]),
    {error, euser} = ftp:user(Ftp, "test", "test2"),
    ftp:close(Ftp),
    inets:stop(ftpd, Pid).


