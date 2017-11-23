%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Simple poolboy wrapper around katja
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(katja_pool).

%%%_* Exports ==========================================================
-export([ send_event/1
        , send_event/2
        , send_event_async/1
        , send_event_async/2
        , send_event_async/3
        , send_events/1
        , send_events/2
        , send_events_async/1
        , send_events_async/2
        , send_events_async/3
        , send_state/1
        , send_state/2
        , send_state_async/1
        , send_state_async/2
        , send_state_async/3
        , send_states/1
        , send_states/2
        , send_states_async/1
        , send_states_async/2
        , send_states_async/3
        , send_entities/1
        , send_entities/2
        , send_entities_async/1
        , send_entities_async/2
        , send_entities_async/3
        , query/1
        , query_async/1
        , query_event/1
        , query_event_async/1
        ]).

%%%_* Types ============================================================
-type event()       :: katja:event().
-type state()       :: katja:state().
-type entities()    :: katja:entities().
-type sample_rate() :: katja:sample_rate().
-type transport()   :: katja_connection:transport().

%%%_* API ==============================================================

%% @doc Delegates to {@link send_event/3}. `Transport' is set to `config'.
-spec send_event(event()) -> ok | {error, term()}.
send_event(Data) ->
  send_event(config, Data).

%% @doc Sends a single event to Riemann.
%%      Delegates to {@link katja:send_event/3}.
-spec send_event(transport(), event()) -> ok | {error, term()}.
send_event(Transport, Data) ->
  with_writer_pool(
    fun(Worker) -> katja:send_event(Worker, Transport, Data)
    end).

%% @doc Delegates to {@link send_event_async/3}. `Transport' is set to `config'.
-spec send_event_async(event()) -> ok.
send_event_async(Data) ->
  send_event_async(config, Data).

%% @doc Delegates to {@link send_event_async/4}. `SampleRate' is set to `1.0'.
-spec send_event_async(transport(), event()) -> ok.
send_event_async(Transport, Data) ->
  send_event_async(Transport, Data, 1.0).

%% @doc Sends a single event to Riemann asynchronously.
%%      Delegates to {@link katja:send_event_async/4}.
-spec send_event_async(transport(), event(), sample_rate()) -> ok.
send_event_async(Transport, Data, SampleRate) ->
  with_writer_pool(
    fun(Worker) ->
        katja:send_event_async(Worker, Transport, Data, SampleRate)
    end).

%% @doc Delegates to {@link send_events/3}. `Transport' is set to `config'.
-spec send_events([event()]) -> ok | {error, term()}.
send_events(Data) ->
  send_events(config, Data).

%% @doc Sends multiple events to Riemann.
%%      Simple wrapper around {@link send_entities/3}.
-spec send_events(transport(), [event()]) -> ok | {error, term()}.
send_events(Transport, Data) ->
  send_entities(Transport, [{events, Data}]).

%% @doc Delegates to {@link send_events_async/3}.
%%      `Transport' is set to `config'.
-spec send_events_async([event()]) -> ok.
send_events_async(Data) ->
  send_events_async(config, Data).

%% @doc Delegates to {@link send_events_async/4}. `SampleRate' is set to `1.0'.
-spec send_events_async(transport(), [event()]) -> ok.
send_events_async(Transport, Data) ->
  send_events_async(Transport, Data, 1.0).

%% @doc Sends multiple events to Riemann asynchronously.
%%      Simple wrapper around {@link send_entities_async/3}.
-spec send_events_async(transport(), [event()], sample_rate()) -> ok.
send_events_async(Transport, Data, SampleRate) ->
  send_entities_async(Transport, [{events, Data}], SampleRate).

%% @doc Delegates to {@link send_state/3}. `Transport' is set to `config'.
-spec send_state(state()) -> ok | {error, term()}.
send_state(Data) ->
  send_state(config, Data).

%% @doc Sends a single state to Riemann.
%%      Delegates to {@link katja:send_state/3}.
-spec send_state(transport(), state()) -> ok | {error, term()}.
send_state(Transport, Data) ->
  with_writer_pool(
    fun(Worker) ->
        katja:send_state(Worker, Transport, Data)
    end).

%% @doc Delegates to {@link send_state_async/2}.
-spec send_state_async(state()) -> ok.
send_state_async(Data) ->
  send_state_async(config, Data).

%% @doc Delegates to {@link send_state_async/4}. `SampleRate' is set to `1.0'.
-spec send_state_async(transport(), state()) -> ok.
send_state_async(Transport, Data) ->
  send_state_async(Transport, Data, 1.0).

%% @doc Sends a single state to Riemann asynchronously.
%%      Delegates to {@link katja:send_state_async/4}.
-spec send_state_async(transport(), state(), sample_rate()) -> ok.
send_state_async(Transport, Data, SampleRate) ->
  with_writer_pool(
    fun(Worker) ->
        katja:send_state_async(Worker, Transport, Data, SampleRate)
    end).

%% @doc Delegates to {@link send_states/3}. `Transport' is set to `config'.
-spec send_states([state()]) -> ok | {error, term()}.
send_states(Data) ->
  send_states(config, Data).

%% @doc Sends multiple states to Riemann.
%%      Simple wrapper around {@link send_entities/2}.
-spec send_states(transport(), [state()]) -> ok | {error, term()}.
send_states(Transport, Data) ->
  send_entities(Transport, [{states, Data}]).

%% @doc Delegates to {@link send_states_async/3}.
%%      `Transport' is set to `config'.
-spec send_states_async([state()]) -> ok.
send_states_async(Data) ->
  send_states_async(config, Data).

%% @doc Delegates to {@link send_states_async/4}. `SampleRate' is set to `1.0'.
-spec send_states_async(transport(), [state()]) -> ok.
send_states_async(Transport, Data) ->
  send_states_async(Transport, Data, 1.0).

%% @doc Sends multiple states to Riemann asynchronously.
%%      Simple wrapper around {@link send_entities_async/3}.
-spec send_states_async(transport(), [state()], sample_rate()) -> ok.
send_states_async(Transport, Data, SampleRate) ->
  send_entities_async(Transport, [{states, Data}], SampleRate).

%% @doc Delegates to {@link send_entities/3}. `Transport' is set to `config'.
-spec send_entities(entities()) -> ok | {error, term()}.
send_entities(Data) ->
  send_entities(config, Data).

%% @doc Sends multiple entities (events and/or states) to Riemann.
%%      Delegates to {@link katja:send_entities/3}.
-spec send_entities(transport(), entities()) -> ok | {error, term()}.
send_entities(Transport, Data) ->
  with_writer_pool(
    fun(Worker) ->
        katja:send_entities(Worker, Transport, Data)
    end).

%% @doc Delegates to {@link send_entities_async/3}.
%%      `Transport' is set to `config'.
-spec send_entities_async(entities()) -> ok.
send_entities_async(Data) ->
  send_entities_async(config, Data).

%% @doc Delegates to {@link send_entities_async/4}.
%%      `SampleRate' is set to `1.0'.
-spec send_entities_async(transport(), entities()) -> ok.
send_entities_async(Transport, Data) ->
  send_entities_async(Transport, Data, 1.0).

%% @doc Sends multiple entities (events and/or states) to Riemann
%%      asynchronously. Delegates to {@link katja:send_entities_async/4}.
-spec send_entities_async(transport(), entities(), sample_rate()) -> ok.
send_entities_async(Transport, Data, SampleRate) ->
  with_writer_pool(
    fun(Worker) ->
        katja:send_entities_async(Worker, Transport, Data, SampleRate)
    end).

%% @doc Delegates to {@link katja:query/2}.
-spec query(string()) -> {ok, [event()]} | {error, term()}.
query(Query) ->
  with_reader_pool(fun(Worker) -> katja:query(Worker, Query) end).

%% @doc Delegates to {@link katja:query_async/2}.
-spec query_async(string()) -> reference().
query_async(Query) ->
  with_reader_pool(fun(Worker) -> katja:query_async(Worker, Query) end).

%% @doc Delegates to {@link katja:query_event/2}.
-spec query_event(event()) -> {ok, [event()]} | {error, term()}.
query_event(Event) ->
  with_reader_pool(fun(Worker) -> katja:query_event(Worker, Event) end).

%% @doc Delegates to {@link katja:query_event_async/2}.
-spec query_event_async(event()) -> reference().
query_event_async(Event) ->
  with_reader_pool(fun(Worker) -> katja:query_event_async(Worker, Event) end).

%%%_* Internal =========================================================
with_writer_pool(Fun) ->
  poolboy:transaction(katja_writer_pool, Fun).

with_reader_pool(Fun) ->
  poolboy:transaction(katja_reader_pool, Fun).

%%%_* Editor ===========================================================
%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
