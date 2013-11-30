-module(main_socket).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-include("types.hrl").
-record(state, {
  server :: #socket_info{},
  clients=[],
  message_handler,
	messages=[]}).
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, send_to_last_accepted/1]).
%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, receive_from_client/1, accept/1, handle_message/2]).


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
-spec(start_link(#socket_info{}) -> pid()).
start_link(MainSocket) ->
		logger:info("start_link: ~w~n", [?MODULE]),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [MainSocket], []).

send_to_last_accepted(Bytes) ->
	gen_server:call(?SERVER, {send_to_last_accepted, Bytes}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init([#socket_info{port = Port} = SocketInfo]) ->
	logger:info("init: ~w~n", [?MODULE]),
	logger:info("Server: ~p~n", [SocketInfo]),
  {ok, SocketServer} = gen_tcp:listen(Port, [binary, {packet, 0},{active, false}]),
	logger:info("Started server on port: ~p. ~p~n", [Port, SocketServer]),
	Pid = spawn_link(?SERVER, accept, [SocketServer]),
	logger:info("Accept Pid: ~p~n", [Pid]),
   {ok, #state{
	  	server = SocketServer
	 }}.

%% ------------------------------------------------------------------
%% Пока сокет жив  - ожидаем подключения
%% ------------------------------------------------------------------
-spec(accept(#socket_info{}) -> {error, _}).
accept(SocketServer) ->
	logger:info("Begin accept: ~p~n", [SocketServer]),
  Res = gen_tcp:accept(SocketServer),
	case Res of
		{ok, SocketClient} ->
				logger:info("Accepted client: ~p~n", [SocketClient]),
  			gen_server:cast(?SERVER, {accept_socket_client, SocketClient}),
  			spawn(?SERVER, receive_from_client, [SocketClient]),
				accept(SocketServer);
		{error, Reason} ->
			logger:info("Error: ~w~n",[{Reason}]),
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
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% ------------------------------------------------------------------
handle_cast({closed_client, SocketClient}, #state{} = State) ->
	logger:info("Client disconnected: ~w~n", [SocketClient]),
	{noreply, State};
handle_cast({receive_from_client, SocketClient, NewBytes}, #state{message_handler = Handler, messages = Messages} = State) ->
  logger:info("Receive message from ~w: ~w~n", [SocketClient, NewBytes]),
	case handle_message(Handler, NewBytes) of
		{undefined, handler_is_absent} ->
			NewState =State#state{messages = [{NewBytes}, Messages]},
  		{noreply, NewState};
		_ ->
			{noreply, State}
	end;
handle_cast({accept_socket_client, SocketClient}, #state{clients=Clients} = State) ->
	NewClients=[SocketClient | Clients],
  logger:info("Active clients [~w]: ~w~n", [length(NewClients), {NewClients}]),
  NewState = State#state{
   clients=NewClients
  },
  {noreply, NewState};

handle_cast(_Msg, State) ->
		logger:info("handle_cast: unknown"),
    {noreply, State}.

%% ------------------------------------------------------------------
handle_info(_Info, State) ->
		logger:info("handle_info: unknown"),
    {noreply, State}.

terminate(_Reason, _State) ->
		logger:info("terminate: unknown"),
    ok.

code_change(_OldVsn, State, _Extra) ->
		logger:info("code_change: unknown"),
    {ok, State}.

%% ------------------------------------------------------------------
%% Если обработчик события есть - обрабатываем
%% ------------------------------------------------------------------
handle_message(Handler, Bytes) ->
	case Handler of
		undefined -> {undefined, handler_is_absent};
		_ -> Handler(Bytes)
	end.
