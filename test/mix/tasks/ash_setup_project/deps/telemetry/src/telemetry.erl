-module(telemetry).

-export([attach/4,
         attach_many/4,
         detach/1,
         list_handlers/1,
         execute/2,
         execute/3,
         span/3]).

-export([report_cb/1]).

-include("telemetry.hrl").

?MODULEDOC("""
`telemetry` allows you to invoke certain functions whenever a
particular event is emitted.

For more information see the documentation for `attach/4`, `attach_many/4`
and `execute/2`.
""").

-type handler_id() :: term().
-type event_name() :: [atom(), ...].
-type event_measurements() :: map().
-type event_metadata() :: map().
-type event_value() :: number().
-type event_prefix() :: [atom()].
-type handler_config() :: term().
-type handler_function() :: fun((event_name(), event_measurements(), event_metadata(), handler_config()) -> any()).
-type span_result() :: term().
-type span_function() :: fun(() -> {span_result(), event_metadata()}) | {span_result(), event_measurements(), event_metadata()}.
-type handler() :: #{id := handler_id(),
                     event_name := event_name(),
                     function := handler_function(),
                     config := handler_config()}.

-export_type([handler_id/0,
              event_name/0,
              event_measurements/0,
              event_metadata/0,
              event_value/0,
              event_prefix/0,
              handler_config/0,
              handler_function/0,
              handler/0,
              span_result/0,
              span_function/0]).

-import_lib("kernel/import/logger.hrl").

?DOC("""
Attaches the handler to the event.

`HandlerId` must be unique, if another handler with the same ID already exists the
`{error, already_exists}` tuple is returned.

See `execute/3` to learn how the handlers are invoked.

> #### Function Captures {: .info}
>
> Due to how anonymous functions are implemented in the Erlang VM, it is best to use
> function captures (`fun mod:fun/4` in Erlang or `&Mod.fun/4` in Elixir) as event handlers
> to achieve the best performance. In other words, avoid using literal anonymous functions
> (`fun(...) -> ... end` or `fn ... -> ... end`) or local function captures (`fun handle_event/4`
> or `&handle_event/4`) as event handlers.

All the handlers are executed by the process dispatching event. If the function fails (raises,
exits or throws) then the handler is removed and a failure event is emitted.

Handler failure events `[telemetry, handler, failure]` should only be used for monitoring
and diagnostic purposes. Re-attaching a failed handler will likely result in the handler
failing again.

Note that you should not rely on the order in which handlers are invoked.
""").
-spec attach(HandlerId, EventName, Function, Config) -> ok | {error, already_exists} when
      HandlerId :: handler_id(),
      EventName :: event_name(),
      Function :: handler_function(),
      Config :: handler_config().
attach(HandlerId, EventName, Function, Config) ->
    attach_many(HandlerId, [EventName], Function, Config).

?DOC("""
Attaches the handler to many events.

The handler will be invoked whenever any of the events in the `EventNames` list is emitted. Note
that failure of the handler on any of these invocations will detach it from all the events in
`EventNames` (the same applies to manual detaching using `detach/1`).

<b>Note:</b> due to how anonymous functions are implemented in the Erlang VM, it is best to use
function captures (i.e. `fun mod:fun/4` in Erlang or `&Mod.fun/4` in Elixir) as event handlers
to achieve maximum performance. In other words, avoid using literal anonymous functions
(`fun(...) -> ... end` or `fn ... -> ... end`) or local function captures (`fun handle_event/4`
or `&handle_event/4`) as event handlers.

All the handlers are executed by the process dispatching event. If the function fails (raises,
exits or throws) a handler failure event is emitted and then the handler is removed.

Handler failure events `[telemetry, handler, failure]` should only be used for monitoring
and diagnostic purposes. Re-attaching a failed handler will likely result in the handler
failing again.

Note that you should not rely on the order in which handlers are invoked.
""").
-spec attach_many(HandlerId, [EventName], Function, Config) -> ok | {error, already_exists} when
      HandlerId :: handler_id(),
      EventName :: event_name(),
      Function :: handler_function(),
      Config :: handler_config().
attach_many(HandlerId, EventNames, Function, Config) when is_function(Function, 4) ->
    assert_event_names(EventNames),
    case erlang:fun_info(Function, type) of
        {type, external} ->
            ok;
        {type, local} ->
            ?LOG_INFO(#{handler_id => HandlerId,
                        event_names => EventNames,
                        function => Function,
                        config => Config,
                        type => local},
                      #{report_cb => fun ?MODULE:report_cb/1})
    end,
    telemetry_handler_table:insert(HandlerId, EventNames, Function, Config).

?DOC("""
Removes the existing handler.

If the handler with given ID doesn't exist, `{error, not_found}` is returned.
""").
-spec detach(handler_id()) -> ok | {error, not_found}.
detach(HandlerId) ->
    telemetry_handler_table:delete(HandlerId).

?DOC("""
Emits the event, invoking handlers attached to it.

When the event is emitted, the handler function provided to `attach/4` is called with four
arguments:

  * the event name
  * the map of measurements
  * the map of event metadata
  * the handler configuration given to `attach/4`

#### Best practices and conventions:

While you are able to emit messages of any `t:event_name/0` structure, it is recommended that you follow the
the guidelines laid out in `span/3` if you are capturing start/stop events.
""").
-spec execute(EventName, Measurements, Metadata) -> ok when
      EventName :: event_name(),
      Measurements :: event_measurements() | event_value(),
      Metadata :: event_metadata().
execute(EventName, Value, Metadata) when is_number(Value) ->
    ?LOG_WARNING("Using execute/3 with a single event value is deprecated. "
                 "Use a measurement map instead.", []),
    execute(EventName, #{value => Value}, Metadata);
execute([_ | _] = EventName, Measurements, Metadata) when is_map(Measurements) and is_map(Metadata) ->
    Handlers = telemetry_handler_table:list_for_event(EventName),
    ApplyFun =
        fun(#handler{id=HandlerId,
                     function=HandlerFunction,
                     config=Config}) ->
            try
                HandlerFunction(EventName, Measurements, Metadata, Config)
            catch
                ?WITH_STACKTRACE(Class, Reason, Stacktrace)
                    detach(HandlerId),
                    FailureMetadata = #{event_name => EventName,
                                        handler_id => HandlerId,
                                        handler_config => Config,
                                        kind => Class,
                                        reason => Reason,
                                        stacktrace => Stacktrace},
                    FailureMeasurements = #{monotonic_time => erlang:monotonic_time(), system_time => erlang:system_time()},
                    execute([telemetry, handler, failure], FailureMeasurements, FailureMetadata),
                    ?LOG_ERROR("Handler ~p has failed and has been detached. "
                               "Class=~p~nReason=~p~nStacktrace=~p~n",
                               [HandlerId, Class, Reason, Stacktrace])
            end
        end,
    lists:foreach(ApplyFun, Handlers).

?DOC("""
Runs the provided `SpanFunction`, emitting start and stop/exception events, invoking the handlers attached to each.

The `SpanFunction` must return a `{result, stop_metadata}` or a `{result, extra_measurements, stop_metadata}` tuple.

When this function is called, 2 events will be emitted via `execute/3`. Those events will be one of the following
pairs:

  * `EventPrefix ++ [start]` and `EventPrefix ++ [stop]`
  * `EventPrefix ++ [start]` and `EventPrefix ++ [exception]`

However, note that in case the current process crashes due to an exit signal
of another process, then none or only part of those events would be emitted.
Below is a breakdown of the measurements and metadata associated with each individual event.

When providing `StartMetadata` and `StopMetadata`, these values will be sent independently to `start` and
`stop` events. If an exception occurs, exception metadata will be merged onto the `StartMetadata`. In general,
it is **highly recommended** that `StopMetadata` should include the values from `StartMetadata`
so that handlers, such as those used for metrics, can rely entirely on the `stop` event. Failure to include
all of `StartMetadata` in `StopMetadata` can add significant complexity to event handlers.

A default span context is added to event metadata under the `telemetry_span_context` key if this key is not provided
by the user in the `StartMetadata`. This context is useful for tracing libraries to identify unique
executions of span events within a process to match start, stop, and exception events. Metadata keys which
should be available to both `start` and `stop` events need to supplied separately for `StartMetadata` and
`StopMetadata`.

If `SpanFunction` returns `{result, extra_measurements, stop_metadata}`, then a map of extra measurements
will be merged with the measurements automatically provided. This is useful if you want to return, for example,
bytes from an HTTP request. The standard measurements `duration` and `monotonic_time` cannot be overridden.

For `telemetry` events denoting the **start** of a larger event, the following data is provided:

  * Event:

    ```
    EventPrefix ++ [start]
    ```

  * Measurements:

    ```
    #{
      % The current system time in native units from
      % calling: erlang:system_time()
      system_time => integer(),
      monotonic_time => integer(),
    }
    ```

  * Metadata:

    ```
    #{
      telemetry_span_context => term(),
      % User defined metadata as provided in StartMetadata
      ...
    }
    ```



For `telemetry` events denoting the **stop** of a larger event, the following data is provided:

  * Event:

    ```
    EventPrefix ++ [stop]
    ```

  * Measurements:

    ```
    #{
      % The current monotonic time minus the start monotonic time in native units
      % by calling: erlang:monotonic_time() - start_monotonic_time
      duration => integer(),
      monotonic_time => integer(),
      % User defined measurements when returning `SpanFunction` as a 3 element tuple
    }
    ```

  * Metadata:

    ```
    #{
      % An optional error field if the stop event is the result of an error
      % but not necessarily an exception.
      error => term(),
      telemetry_span_context => term(),
      % User defined metadata as provided in StopMetadata
      ...
    }
    ```

For `telemetry` events denoting an **exception** of a larger event, the following data is provided:

  * Event:

    ```
    EventPrefix ++ [exception]
    ```

  * Measurements:

    ```
    #{
      % The current monotonic time minus the start monotonic time in native units
      % by calling: erlang:monotonic_time() - start_monotonic_time
      duration => integer(),
      monotonic_time => integer()
    }
    ```

  * Metadata:

    ```
    #{
      kind => throw | error | exit,
      reason => term(),
      stacktrace => list(),
      telemetry_span_context => term(),
      % User defined metadata as provided in StartMetadata
       ...
    }
    ```

""").
-spec span(event_prefix(), event_metadata(), span_function()) -> span_result().
span(EventPrefix, StartMetadata, SpanFunction) ->
    StartTime = erlang:monotonic_time(),
    DefaultCtx = erlang:make_ref(),
    execute(
        EventPrefix ++ [start],
        #{monotonic_time => StartTime, system_time => erlang:system_time()},
        merge_ctx(StartMetadata, DefaultCtx)
    ),

    try SpanFunction() of
      {Result, StopMetadata} ->
          StopTime = erlang:monotonic_time(),
          execute(
              EventPrefix ++ [stop],
              #{duration => StopTime - StartTime, monotonic_time => StopTime},
              merge_ctx(StopMetadata, DefaultCtx)
          ),
          Result;
      {Result, ExtraMeasurements, StopMetadata} ->
          StopTime = erlang:monotonic_time(),
          Measurements = maps:merge(ExtraMeasurements, #{duration => StopTime - StartTime, monotonic_time => StopTime}),
          execute(
              EventPrefix ++ [stop],
              Measurements,
              merge_ctx(StopMetadata, DefaultCtx)
          ),
          Result

    catch
        ?WITH_STACKTRACE(Class, Reason, Stacktrace)
            StopTime = erlang:monotonic_time(),
            execute(
                EventPrefix ++ [exception],
                #{duration => StopTime - StartTime, monotonic_time => StopTime},
                merge_ctx(StartMetadata#{kind => Class, reason => Reason, stacktrace => Stacktrace}, DefaultCtx)
            ),
            erlang:raise(Class, Reason, Stacktrace)
    end.

?DOC("""
Same as [`execute(EventName, Measurements, #{})`](`execute/3`).
""").
-spec execute(EventName, Measurements) -> ok when
      EventName :: event_name(),
      Measurements :: event_measurements() | event_value().
execute(EventName, Measurements) ->
    execute(EventName, Measurements, #{}).

?DOC("""
Returns all handlers attached to events with given prefix.

Handlers attached to many events at once using `attach_many/4` will be listed once for each
event they're attached to.
Note that you can list all handlers by feeding this function an empty list.
""").
-spec list_handlers(event_prefix()) -> [handler()].
list_handlers(EventPrefix) ->
    assert_event_prefix(EventPrefix),
    [#{id => HandlerId,
       event_name => EventName,
       function => Function,
       config => Config} || #handler{id=HandlerId,
                                     event_name=EventName,
                                     function=Function,
                                     config=Config} <- telemetry_handler_table:list_by_prefix(EventPrefix)].

%%

-spec assert_event_names(term()) -> [ok].
assert_event_names(List) when is_list(List) ->
    [assert_event_name(E) || E <- List];
assert_event_names(Term) ->
    erlang:error(badarg, Term).

-spec assert_event_prefix(term()) -> ok.
assert_event_prefix(List) when is_list(List) ->
    case lists:all(fun erlang:is_atom/1, List) of
        true ->
            ok;
        false ->
            erlang:error(badarg, List)
    end;
assert_event_prefix(List) ->
    erlang:error(badarg, List).

-spec assert_event_name(term()) -> ok.
assert_event_name([_ | _] = List) ->
    case lists:all(fun erlang:is_atom/1, List) of
        true ->
            ok;
        false ->
            erlang:error(badarg, List)
    end;
assert_event_name(Term) ->
    erlang:error(badarg, Term).

-spec merge_ctx(event_metadata(), any()) -> event_metadata().
merge_ctx(#{telemetry_span_context := _} = Metadata, _Ctx) -> Metadata;
merge_ctx(Metadata, Ctx) -> Metadata#{telemetry_span_context => Ctx}.

?DOC(false).
report_cb(#{handler_id := Id}) ->
    {"The function passed as a handler with ID ~w is a local function.\n"
     "This means that it is either an anonymous function or a capture of a function "
     "without a module specified. That may cause a performance penalty when calling "
     "that handler. For more details see the note in `telemetry:attach/4` "
     "documentation.\n\n"
     "https://hexdocs.pm/telemetry/telemetry.html#attach/4", [Id]}.
