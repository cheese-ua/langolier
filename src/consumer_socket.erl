%% Copyright
-module(consumer_socket).
-author("cheese").

-include("types.hrl").
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	code_change/3]).

%% API
-spec(start_link(#socket_info{}) -> 'ignore' | {'error',_} | {'ok',pid()}).
start_link(Socket) ->
  logger:info("start_link: ~w~n", [?MODULE]),
  #socket_info{name = Name} = Socket,
  logger:info("ConsumerSocket ~w: ~p~n", [Name, Socket]),
	gen_server:start_link({local, Name}, ?MODULE, [Socket], []).

%% gen_server callbacks
-record(state, {}).

-spec(init(#socket_info{}) -> {ok,#state{}}).
init([Socket]) ->
  logger:info("init socket_info?: ~w, ~p~n", [?MODULE, Socket]),
  consumer_control:register(#consumer_info{name = Socket#socket_info.name, pid = self()}),
	{ok, #state{}}.

handle_call(_Request, _From, State) ->
	{noreply, State}.

handle_cast(_Request, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
