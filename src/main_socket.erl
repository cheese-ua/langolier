-module(main_socket).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(LOG_FILE, "log/main_socket.log").
-include("../include/types.hrl").
-record(state, {
  server :: #socket_info{},
  server_instance,
  clients=[],
  message_handler}).
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2, send_to_last_accepted/1, test/0]).
%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, receive_from_client/1, accept/1, handle_message/2,
          delete_client/3]).


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link(MainSocket, Handler) ->
    logger:register_file(?LOG_FILE),
		logger:info("start_link: ~w~n", [?MODULE], ?LOG_FILE),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [MainSocket, Handler], []).

send_to_last_accepted(Bytes) ->
	gen_server:call(?SERVER, {send_to_last_accepted, Bytes}).

test() ->
  gen_server:cast(?SERVER, test).
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init([SocketInfo, Handler]) ->
  gen_server:cast(?SERVER, {init, SocketInfo, Handler}),
  {ok, #state{
    server = SocketInfo
  }}.


%% ------------------------------------------------------------------
%% Пока сокет жив  - ожидаем подключения
%% ------------------------------------------------------------------
-spec(accept(port()) -> {stopped, atom()}).
accept(SocketServer) ->
	logger:info("Begin accept: ~p~n", [SocketServer], ?LOG_FILE),
  Res = gen_tcp:accept(SocketServer),
  logger:info("Res: ~p~n", [Res], ?LOG_FILE),
	case Res of
		{ok, SocketClient} ->
      {ok,{Ip,Port}} = inet:peername(SocketClient),
				logger:info("Accepted client ~p: ~w:~w~n", [SocketClient, Ip,Port], ?LOG_FILE),
  			gen_server:cast(?SERVER, {accept_socket_client, SocketClient}),
  			spawn(?SERVER, receive_from_client, [SocketClient]),
				accept(SocketServer);
		{error, Reason} ->
			logger:info("Error: ~w~n",[{Reason}], ?LOG_FILE),
			{stopped, Reason}
		end.

%% ------------------------------------------------------------------
receive_from_client(SocketClient) ->
  case gen_tcp:recv(SocketClient, 0) of
    {ok, Bytes} ->
      gen_server:cast(?SERVER, {receive_from_client, SocketClient, Bytes}),
      receive_from_client(SocketClient);
    {error, closed} ->
			gen_server:cast(?SERVER, {closed_client, SocketClient}),
      {error, closed}
  end.

%% ------------------------------------------------------------------
handle_call({send_to_last_accepted, Bytes}, _From, #state{ clients = Clients}= State) ->
  Res = case Clients of
    [] ->
      logger:info("Clients is absent message ignored: ~p~n", [Bytes], ?LOG_FILE),
      {error, no_clients};
    [Head | _] ->
      {ok,{Ip,Port}} = inet:peername(Head),
      logger:info("Send to ~p:~w: ~w~n", [Ip, Port, Bytes], ?LOG_FILE),
      ResSend = case gen_tcp:send(Head, Bytes) of
              ok ->
                logger:info("Send ok", ?LOG_FILE),
                ok;
              {error, Reason} ->
                logger:info("Send error: ~p~n", [Reason], ?LOG_FILE),
                {error, Reason}
            end,
      ResSend
  end,
  {reply, Res, State};
%% ------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% ------------------------------------------------------------------
handle_cast({init, SocketInfo, Handler}, _State) ->
  socket_utilites:timeout_seconds(1000),
  logger:info("init: ~w~n", [?MODULE], ?LOG_FILE),
  logger:info("Server: ~p~n", [SocketInfo], ?LOG_FILE),
  #socket_info{port = Port, ip = IP} = SocketInfo,
  case gen_tcp:listen(Port, [binary, {packet, 0},{active, false}, {reuseaddr, true}, {ip, IP}])  of
    {ok, SocketServer} ->
      logger:info("Started server on port: ~p. ~p~n", [Port, SocketServer], ?LOG_FILE),
      Pid = spawn(?SERVER, accept, [SocketServer]),
      logger:info("Accept Pid: ~p~n", [Pid], ?LOG_FILE),
      {noreply, #state{server = SocketInfo, message_handler = Handler, server_instance = SocketServer }};
    {error, Reason} ->
      logger:info("Error starting listen: ~p~n", [Reason], ?LOG_FILE),
      error(Reason)
  end;
handle_cast({closed_client, SocketClient}, #state{clients = Clients} = State) ->
	logger:info("Client disconnected: ~w~n", [SocketClient], ?LOG_FILE),
  NewClients = delete_client(SocketClient, [], Clients),
  logger:info("New Clients: ~w~n", [NewClients], ?LOG_FILE),
	{noreply, State#state{clients = NewClients}};
handle_cast({receive_from_client, SocketClient, NewBytes}, #state{message_handler = Handler} = State) ->
  {ok,{Ip,Port}} = inet:peername(SocketClient),
  logger:info("Receive message from ~p:~w: ~w~n", [Ip,Port, NewBytes], ?LOG_FILE),
	handle_message(Handler, NewBytes),
  {noreply, State};
handle_cast({accept_socket_client, SocketClient}, #state{clients=Clients} = State) ->
	NewClients=[SocketClient | Clients],
  logger:info("Active clients [~w]: ~w~n", [length(NewClients), NewClients], ?LOG_FILE),
  NewState = State#state{
   clients=NewClients
  },
  {noreply, NewState}.

%% ------------------------------------------------------------------
handle_info(test, State) ->
  logger:info("State: ~p~n", [State], ?LOG_FILE),
  {noreply, State};
handle_info(_Info, State) ->
		logger:info("handle_info: unknown", ?LOG_FILE),
    {noreply, State}.

terminate(_Reason, _State) ->
		logger:info("terminate: unknown", ?LOG_FILE),
    ok.

code_change(_OldVsn, State, _Extra) ->
		logger:info("code_change: unknown", ?LOG_FILE),
    {ok, State}.

%% ------------------------------------------------------------------
%% Если обработчик события есть - обрабатываем
%% ------------------------------------------------------------------
handle_message(Handler, Bytes) ->
	case Handler of
		undefined -> {undefined, handler_is_absent};
		_ ->
      try Handler(Bytes) of
           Res -> Res
      catch
        ErrRes->
          logger:info("Main handler error: ~w~n", [ErrRes], ?LOG_FILE),
          ErrRes
      end
	end.

delete_client(_SocketClient, NewList, []) ->
  NewList;
delete_client(SocketClient, NewList, [SocketClient | Tail]) ->
  delete_client(SocketClient, NewList, Tail);
delete_client(SocketClient, NewList, [Head | Tail]) ->
  delete_client(SocketClient, [Head | NewList], Tail).

