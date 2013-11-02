-module(langolier_sup).

-behaviour(supervisor).

-export([start_link/2, init/1]).

start_link(MainSocket, ClientsSockets) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [MainSocket, ClientsSockets]).


init([MainSocket, ClientsSockets]) ->
    RestartStrategy = one_for_one, % one_for_one | one_for_all | rest_for_one
    MaxRestarts = 0,
    MaxSecondsBetweenRestarts = 10,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent, % permanent | transient | temporary
    Shutdown = 2000,     % brutal_kill | int() >= 0 | infinity

    SomeWorker = {top_handler,
		  {top_handler, start_link, [MainSocket, ClientsSockets]},
		  Restart, Shutdown, worker, 
		  [top_handler]},

    {ok, {SupFlags, [SomeWorker]}}.

