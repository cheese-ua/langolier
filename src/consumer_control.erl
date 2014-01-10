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
-export([start_link/0, delete_consumer/3, register/1]).

%% gen_server callbacks
-export([init/1,  handle_call/3,  handle_cast/2,  handle_info/2,  terminate/2,  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {consumers :: [#consumer_info{}] }).

%%%===================================================================
%%% API
%%%===================================================================
-spec(register(#consumer_info{}) -> ok).
register(Consumer) ->
  gen_server:cast(?SERVER, {register, Consumer}),
  ok.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
start_link() ->
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
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
handle_cast({register, OneConsumer}, #state{consumers = Consumers} = State) ->
  logger:info("register consumer: ~w~n", [OneConsumer]),
  %consumer_info
  NewConsumers = delete_consumer(OneConsumer, Consumers, []),
  {noreply, State#state{consumers = [OneConsumer | NewConsumers]}};
%%--------------------------------------------------------------------
handle_cast(Request, State) ->
  logger:info("unknown request: ~w~n", [Request]),
  {noreply, State}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  logger:info("Info: ~w~n", [State]),
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


