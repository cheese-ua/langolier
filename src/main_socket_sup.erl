%% Copyright
-module(main_socket_sup).
-author("cheese").

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% supervisor
-export([init/1]).

%% API
start_link(MainSocket) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, [MainSocket]).

%% supervisor callbacks
init([MainSocket]) ->
	RestartStrategy = one_for_one,
	MaxRestarts = 0,
	MaxSecondsBetweenRestarts = 10,
	SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

	Restart = permanent, % permanent | transient | temporary
	Shutdown = 5,     % brutal_kill | int() >= 0 | infinity

	SomeWorker = {main_socket,
		{main_socket, start_link, [MainSocket]},
		Restart, Shutdown, worker,
		[main_socket]},

	{ok, {SupFlags, [SomeWorker]}}.
