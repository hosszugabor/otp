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
-module(ftpd).
-behaviour(inets_service).

-export([
	 start_standalone/1, 
	 start_service/1, 
	 stop_service/1, 
	 services/0, 
	 service_info/1
	]).

-type host()		:: inet:ip_address() |
			   inet:hostname() |
			   any. 				% partly copied from ssl_internal.hrl
-type path()            :: string().                            % copied from ssl.erl
-type ftp_option() 	:: {bind_address, host()} |		% default is any
		      	   {port, inet:port_number()} |		% default is 21
			   {anonymous, boolean()} |		% if anonymous is used, no need for pwd_fun
			   {chrootdir, path()} |		% accept %username too!
			   {pwd_fun, fun()} |			% the default should use the OS, see yaws for inspiration
			   {fd, integer()} |			% if fd is used, no need for bind_address and port
			   {max_clients, integer()} |		% httpd has this option...
			   {trace_fun, fun()} |			% traces FTP commands, file accesses
			   {log_fun, fun()}.			% logs login attempts, there should be a default to log to syslog
-type ftp_config() 	:: [ftp_option()].

-spec start_standalone(Config :: ftp_config()) -> {ok, pid()} | {error, Reason :: term()}.
start_standalone(_) ->
    {error, not_implemented_yet}.

-spec start_service(Config :: ftp_config()) -> {ok, pid()} | {error, Reason :: term()}.
start_service(_) ->
    {error, not_implemented_yet}.

-spec stop_service(Pid :: pid()) -> ok | {error, Reason :: term()}.
stop_service(_) ->
    {error, not_implemented_yet}.

-spec services() -> [{ftpd, pid()}].
services() ->
    [].

-spec service_info(Pid :: pid()) -> 
    {ok, [{Property :: term(), Value :: term()}]} | {error, Reason :: term()}.
service_info(_) ->
    {error, not_implemented_yet}.
