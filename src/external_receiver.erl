%%%-------------------------------------------------------------------
%%% @author cheese
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Jan 2014 2:03 PM
%%%-------------------------------------------------------------------
-module(external_receiver).
-author("cheese").

-behaviour(gen_server).

%% API
-export([start_link/0, external_receiver_loop/0, test/0]).

%% gen_server callbacks
-export([init/1,  handle_call/3,  handle_cast/2,  handle_info/2,  terminate/2,  code_change/3]).

-define(SERVER, ?MODULE).
-define(LOG_FILE, "log/external_receiver.log").

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  logger:info("start_link: ~w~n", [?SERVER], ?LOG_FILE),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


external_receiver_loop() ->
  receive
    Req ->
      logger:info("Req: ~w~n", [Req], ?LOG_FILE),
      external_receiver_loop()
  end.


test() ->
  external_receiver_pid ! test.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  register(external_receiver_pid, spawn(external_receiver, external_receiver_loop, [])),
  logger:info("init: ~w~n", [?SERVER], ?LOG_FILE),
  {ok, #state{}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->

  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
