%% Copyright
-module(consumer_socket).
-author("cheese").

-include("types.hrl").
-behaviour(gen_server).
-define(TIMEOUT_CONNECT, 5000).
-define(TIMEOUT, 5000).
-record(state, {
  handler ,
  socket_info :: #socket_info{},
  socket_instance ,
  server_name :: atom(),
  file_name
}).

%% API
-export([start_link/2, send_message/2, handle_message/2, get_file_name/1]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API
-spec(start_link(#socket_info{}, _) -> 'ignore' | {'error',_} | {'ok',pid()}).
start_link(Socket, Handler) ->
  Name = Socket#socket_info.name,
  LogFileName = get_file_name(Name),
  logger:info("start_link: ~w~n", [?MODULE], LogFileName),
  logger:info("ConsumerSocket ~w: ~p~n", [Name, Socket], LogFileName),
	gen_server:start_link({local, Name}, ?MODULE, [Socket, Handler], []).

send_message(Bytes, ServerName) ->
  gen_server:call(ServerName, {send, Bytes}).


%%%===================================================================
-spec(init([#socket_info{}]) -> {ok,#state{}}).
init([Socket, Handler]) ->
  Name = Socket#socket_info.name,
  LogFileName = get_file_name(Name),
  logger:info("init: ~w, ~p~n", [?MODULE, Socket], LogFileName),
  consumer_control:register(#consumer_info{name = Name, pid = self()}),
  gen_server:cast(Name, {init, Socket, Handler}),
  {ok, #state{}}.

%%%===================================================================
handle_call({send, Bytes}, _From, State) ->
  logger:info("Send to ~p: ~p~n", [State#state.server_name, Bytes], State#state.file_name),
  Res = case gen_tcp:send(State#state.socket_instance, Bytes) of
    ok ->
      logger:info("Send ok: ~p~n", [Bytes], State#state.file_name),
      ok;
    {error, Reason} ->
      logger:info("Send error: ~p. ~p~n", [Reason, Bytes], State#state.file_name),
      {error, Reason}
  end,
  {reply, Res, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%===================================================================
handle_cast({init, SocketInfo, Handler}, State) ->
  Name = SocketInfo#socket_info.name,
  LogFileName = get_file_name(Name),
  Ip = SocketInfo#socket_info.ip,
  Port = SocketInfo#socket_info.port,
  logger:info("Try connect to ~p:~w~n", [Ip, Port], LogFileName),
  Socket = case gen_tcp:connect(Ip, Port,  [binary, {packet, 0}], ?TIMEOUT_CONNECT) of
    {ok, SocketConnected} ->
      SocketConnected;
     {error, Reason} ->
      logger:info("Connect failed: ~w~n", [Reason], LogFileName),
      error(Reason)
  end,
  logger:info("Connected to ~p:~w success: ~w ~n", [Ip, Port, Socket], LogFileName),
  socket_utilites:timeout_seconds(1000),
  NewState = State#state{
    handler = Handler,
     socket_info = SocketInfo,
     socket_instance = Socket,
     file_name = LogFileName,
    server_name = Name
  },
  {noreply, NewState};
%%===================================================================
handle_cast({receive_from_client, Bytes}, State) ->
  logger:info("Receive message: ~w~n", [Bytes], State#state.file_name),
  Handler =State#state.handler,
  handle_message(Handler, Bytes),
  {noreply, State};
%%===================================================================
handle_cast(_Request, State) ->
	{noreply, State}.

%%%===================================================================
handle_info({tcp, RemoteSocket, Bytes}, State) ->
  {ok,{Ip,Port}} = inet:peername(RemoteSocket),
  logger:info("Receive message from ~p:~w: ~w~n", [Ip,Port, Bytes], State#state.file_name),
  {noreply, State};
%%%===================================================================
handle_info({tcp_closed, RemoteSocket}, State) ->
  logger:info("Client disconnected: ~w~n", [RemoteSocket], State#state.file_name),
  socket_utilites:timeout_seconds(?TIMEOUT),
  error(disconnect),
  {noreply, State};
%%%===================================================================
handle_info(_Info, State) ->
	{noreply, State}.

%%%===================================================================
terminate(_Reason, _State) ->
	ok.

%%%===================================================================
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


handle_message(Handler, Bytes) ->
  case Handler of
    undefined -> {undefined, handler_is_absent};
    _ -> Handler(Bytes)
  end.

get_file_name(Name) ->
  "log/" ++ atom_to_list(Name) ++ ".log".
