%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FTPD inner representation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifndef(ftpd_rep_hrl).
-define(ftpd_rep_hrl, true).

%% Control connection data
%%	username ~ actual username set by the user (using USER <loginname>)
%%	authed ~ indicates wether a proper password was given for the actual username
%%	control_sock ~ socket of the control connection
%%	pasv_pid ~ PID of the passive connection used by this control connection
%%		(if exists)
%%	curr_path ~ current directory path
-record(ctrl_conn_data, {username 		= none, 
						authed 			= false, 
						control_socket 	= none, 
						pasv_pid 		= none, 
						curr_path 		= "/",
						pwd_fun			= none,
						chrootdir		= none,
						repr_type		= none			 
						}).

-endif.


