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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FTPD inner representation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifndef(ftpd_rep_hrl).
-define(ftpd_rep_hrl, true).

%% Control connection data
%%	username     ~ actual username set by the user (using USER <loginname>)
%%	authed       ~ indicates whether a proper password
%%                 was given for the actual username
%%	control_sock ~ socket of the control connection
%%	pasv_pid     ~ PID of the passive connection used by this control connection
%%		           (if exists)
%%	curr_path    ~ current directory path
-record(ctrl_conn_data, {control_socket = none,
						chrootdir		= none,
						pwd_fun			= none,
						log_fun			= none,
						trace_fun		= none,
						session_state	= none,

						data_pid 		= none,
						username 		= none,
				  		authed 			= false,
				  		curr_path 		= "/",
				  		repr_type		= none,
						rename_from		= none
				  		}).

%% Defines

-define(UTIL, ftpd_util).
-define(RESP(Comm, Msg), ?UTIL:response(Comm, Msg)).

-define(DEFAULT_PORT, 21).

%% Types

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

-endif.

