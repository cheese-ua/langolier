%% Copyright
-module(consumer_socket_sup).
-author("cheese").

-include("types.hrl").
-behaviour(supervisor).

%% API
-export([start_link/1]).

%% supervisor
-export([init/1]).

%% API
start_link(Socket) ->
  SuperVisorName = socket_utilites:get_name(Socket, "super_"),
	supervisor:start_link({local, SuperVisorName}, ?MODULE, [Socket]).

%% supervisor callbacks
init([Socket]) ->

  InitSocketParam = socket_utilites:parseSocket(Socket),

  SuperVisorName = socket_utilites:get_name(Socket, "super_"),

  logger:info("Start supervisor ~w: ~w, ~p~n", [?MODULE, SuperVisorName, InitSocketParam]),

	SomeWorker = {SuperVisorName,
		{consumer_socket, start_link, [InitSocketParam]},
    permanent, 2, worker,
		[]},

	{ok, {
    {one_for_one, 2, 5},
    [SomeWorker]}}.
