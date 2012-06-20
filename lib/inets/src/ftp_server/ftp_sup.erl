-module(ftp_sup).
-behavior(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(Args) ->
    
    supervisor:start_link(ftp_sup, Args).

init(Args) ->
	    {ok, {{one_for_one, 10, 60},
          [{listener, {listener, start_link, [Args]},
            permanent, brutal_kill, worker, [listener]}]}}
.
