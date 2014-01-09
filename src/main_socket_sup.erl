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
init([SocketParam]) ->
  logger:info("Start supervisor: ~w~n", [?MODULE]),

	InstanceSocket = {main_socket,
		{main_socket, start_link, [socket_utilites:parseSocket(SocketParam)]},
    permanent, 2, worker,
		[]},

	{ok, {
        {one_for_one,2,5},
        [InstanceSocket]
      }
  }.
