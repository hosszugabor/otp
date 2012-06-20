-module(ftpd_sup).
-behavior(supervisor).

-export([start_link/1]).
-export([init/1,stop/1]).

start_link(Args) ->
    supervisor:start_link(ftpd_sup, Args).

stop(Pid) ->
 %   supervisor:terminate_child(Pid,listener),
	exit(Pid,shutdown),
	true
.

init(Args) ->
    erlang:register(ftpd_sup,self()),
	    {ok, {{one_for_one, 10, 60},
          [{ftpd_listener, {ftpd_listener, start_link, [Args]},
            permanent, brutal_kill, worker, [ftpd_listener]}]}}
.
