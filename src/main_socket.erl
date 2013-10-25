-module(main_socket).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {
  server,
  clients = [],
  bytes=[]}).
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, receive_from_client/1]).




%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
-spec(start_link() -> pid()).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init(_Args) ->
  {ok, SocketServer} = gen_tcp:listen(8001, [binary, {packet, 0},{active, false}]),
  gen_server:call(?SERVER, {start_socket_server, SocketServer}),
  accept(SocketServer),
   {ok}.

accept(SocketServer) ->
  {ok, SocketClient} = gen_tcp:accept(SocketServer),
  gen_server:cast(?SERVER, {accept_socket_client, SocketClient}),
  spawn_link(?SERVER, receive_from_client, [SocketClient]),
  accept(SocketServer).

receive_from_client(SocketClient) ->
  case gen_tcp:recv(SocketClient, 0) of
    {ok, Bytes} ->
      gen_server:cast(?SERVER, {receive_from_client, SocketClient, Bytes}),
      receive_from_client(SocketClient);
    {error, closed} ->
      {error, closed}
  end.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({receive_from_client, SocketClient, NewBytes}, #state{bytes=Bytes} = State) ->
  logger:info("Receive message: ~w~n", [NewBytes]),
  NewState = State#state{
    bytes=[{SocketClient, NewBytes} | Bytes]
  },
  {noreply, NewState};
handle_cast({start_socket_server, SocketServer}, State) ->
  logger:info("Start server: ~w~n", [SocketServer]),
  NewState = State#state{
    server=SocketServer
  },
  {noreply, NewState};
handle_cast({accept_socket_client, SocketClient}, #state{clients=Clients} = State) ->
  logger:info("Accept client: ~w~n", [SocketClient]),
  NewState = State#state{
   clients=[SocketClient | Clients]
  },
  {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

