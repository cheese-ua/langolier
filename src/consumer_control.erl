%%%-------------------------------------------------------------------
%%% @author cheese
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Jan 2014 8:53 AM
%%%-------------------------------------------------------------------
-module(consumer_control).
-author("cheese").

-include("types.hrl").
-behaviour(gen_server).

%% API
-export([start_link/0, delete_consumer/3, register/1, get_next_consumer/0, send_message/1, send_echo/0]).

%% gen_server callbacks
-export([init/1,  handle_call/3,  handle_cast/2,  handle_info/2,  terminate/2,  code_change/3]).

-define(SERVER, ?MODULE).
-define(LOG_FILE, "log/consumer_control.log").

-record(state, {
  consumers :: [#consumer_info{}]
}).

%%%===================================================================
%%% API
%%%===================================================================
-spec(register(#consumer_info{}) -> ok).
register(Consumer) ->
  gen_server:cast(?SERVER, {register, Consumer}),
  ok.

-spec(get_next_consumer() -> #consumer_info{} | not_present).
get_next_consumer() ->
  case gen_server:call(?SERVER, get_next_consumer) of
    not_present ->
      not_present;
    {ok, Consumer} ->
      Consumer
  end.

send_echo() ->
  send_message("ping").

send_message(Bytes) ->
  case get_next_consumer() of
    not_present
      -> {failed, not_present };
    Consumer ->
      consumer_socket:send_message(Bytes, Consumer#consumer_info.name)
  end.
%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
start_link() ->
  logger:register_file(?LOG_FILE),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
init([]) ->
  {ok, #state{consumers = []}}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
handle_call(get_next_consumer, _From, State) ->
  Consumers = State#state.consumers,
  case Consumers of
    [] ->
      {reply, not_present, State};
    [Head | Tail] ->
      NewState = State#state{
      consumers = Tail ++ [Head]
       },
      {reply, {ok, Head}, NewState}
  end;
%%--------------------------------------------------------------------
handle_call(Request, _From, State) ->
  logger:info("unknown handle_call: ~w~n", [Request], ?LOG_FILE),
  {reply, ok, State}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
handle_cast({register, OneConsumer}, #state{consumers = Consumers} = State) ->
  logger:info("register consumer: ~w~n", [OneConsumer], ?LOG_FILE),
  NewConsumers = delete_consumer(OneConsumer, Consumers, []),
  {noreply, State#state{consumers = [OneConsumer | NewConsumers]}};
%%--------------------------------------------------------------------
handle_cast(Request, State) ->
  logger:info("unknown handle_cast: ~w~n", [Request], ?LOG_FILE),
  {noreply, State}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  logger:info("Info: ~w~n", [State], ?LOG_FILE),
  {noreply, State}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec(delete_consumer(#consumer_info{}, [#consumer_info{}], [#consumer_info{}]) -> [#consumer_info{}]).
delete_consumer(_OneConsumer, [], Res) ->
  lists:reverse(Res);
delete_consumer(OneConsumer, [Head | Tail], Res) ->
  HeadName = Head#consumer_info.name,
  ConsName = OneConsumer#consumer_info.name,
  case HeadName of
    ConsName ->
      delete_consumer(OneConsumer, Tail, Res);
    _ ->
      delete_consumer(OneConsumer, Tail, [Head | Res])
  end.


