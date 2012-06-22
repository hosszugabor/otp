-module(ftpd_sup).
-behavior(supervisor).

-export([start_link/1]).
-export([init/1,stop/1]).

start_link(Args) ->
    supervisor:start_link(ftpd_sup, Args).

stop(Pid) ->
	exit(Pid,shutdown), %% TODO error reporting
	ok.

init(Args) ->
	NewArgs = [ {sup_pid, self()} | Args],
	{ok, {{one_for_one, 1, 60},
          [{ftpd_listener, {ftpd_listener, start_link, [NewArgs]},
            permanent, 100000, worker, [ftpd_listener]}]}}.
