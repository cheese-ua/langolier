-module(langolier_sup).

-behaviour(supervisor).

-export([start_link/2, init/1, prepare_consumer/2]).

start_link(MainSocket, ClientsSockets) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [MainSocket, ClientsSockets]).


init([MainSocket, ClientsSockets]) ->
		logger:info("Start supervisor: ~w~n", [?MODULE]),

    MainSocketWorker = {main_socket_sup,
		  {main_socket_sup, start_link, [MainSocket]},
      permanent, 2000, supervisor,
		  []},

    ConsumerSockets = prepare_consumer(ClientsSockets, []),

    {ok, {
      {one_for_one, 2, 5},
      [MainSocketWorker | ConsumerSockets]
         }
    }.

prepare_consumer([], Res) ->
  lists:reverse(Res);
prepare_consumer([Socket | Tail], Res) ->
  SuperVisorName = socket_utilites:get_name(Socket, "super_super_"),
  NewRes = [
    {SuperVisorName,
      {consumer_socket_sup, start_link, [Socket]},
      permanent, 2000, supervisor,
      []}     | Res],
  prepare_consumer(Tail, NewRes).
