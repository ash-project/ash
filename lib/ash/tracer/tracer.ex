# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Tracer do
  @moduledoc """
  A behaviour for implementing tracing for an Ash application.
  """

  @typedoc "The operation in a span type of the form `[:authorization, operation]`."
  @type authorization_operation() :: :run_checks | :authorize_related | :runtime_check

  @typedoc """
  Identifies the operation a span measures.

  For example, `:action` measures an action invocation, and
  `[:authorization, :run_checks]` measures its authorization checks. Custom
  spans can use `:custom` or their own type. See the
  [trace types in Monitoring](/documentation/topics/advanced/monitoring.md#trace-types).
  """
  @type span_type() ::
          :action
          | [:authorization | authorization_operation()]
          | :bulk_create
          | :bulk_update
          | :bulk_destroy
          | :bulk_batch
          | :changeset
          | :query
          | :calculate
          | :request_step
          | :change
          | :validation
          | :preparation
          | :custom
          | :notifier
          | :before_transaction
          | :before_action
          | :after_transaction
          | :after_action
          | {:custom, atom()}
          | term()

  @typedoc """
  Attributes associated with a span. The keys depend on the span type.

  Action spans can include the resource, action, actor, tenant, and authorization
  setting. Choose which keys to export when sending metadata to an external
  tracing backend.
  """
  @type metadata() ::
          %{
            domain: nil | module(),
            resource: nil | module(),
            actor: term(),
            tenant: term(),
            action: atom(),
            authorize?: boolean()
          }
          | map()

  @typedoc "A module implementing the `Ash.Tracer` behaviour."
  @type t :: module

  @doc """
  Opens a span for the given type and resolved name in the current process.

  Make it the active span so metadata, errors, and nested spans can be
  associated with it. Use the installed span context to connect the first
  local span to work in another process.
  """
  @callback start_span(span_type(), name :: String.t()) :: :ok

  @doc """
  Finishes the active span and restores its local parent, if any.

  This is where a tracer can record elapsed time and send the completed span
  to its destination. Nested spans finish in reverse order of creation.
  """
  @callback stop_span() :: :ok

  @doc """
  Captures the context needed to continue the current trace in another process.

  This value is defined by the tracer. Ash passes it to `c:set_span_context/1`
  in worker processes when it propagates tracing context.
  """
  @callback get_span_context() :: term()

  @doc """
  Installs a context captured by `c:get_span_context/0` in the current process.

  Use it when starting spans in this process so they can be associated with
  the trace that initiated the work.
  """
  @callback set_span_context(term()) :: :ok

  @doc """
  Records an exception raised while a span is active.

  Ash prefers this callback when both error callbacks are implemented.
  `opts[:stacktrace]` contains the stacktrace when supplied by the caller.
  """
  @callback set_error(Exception.t(), Keyword.t()) :: :ok

  @doc """
  Returns whether the tracer should receive spans of this type.

  Ash treats all types as enabled when this callback is omitted. Match the
  entire list for authorization spans, such as `[:authorization, :run_checks]`.
  """
  @callback trace_type?(span_type()) :: boolean()

  @doc """
  Records an error that Ash handles and returns as an error result.

  Ash invokes this optional callback at action boundaries that report handled
  errors. The options may include `:stacktrace`.
  """
  @callback set_handled_error(Exception.t(), Keyword.t()) :: :ok

  @doc """
  Set metadata for the current span.

  This may be called multiple times per span, and should ideally merge with previous metadata.
  """
  @callback set_metadata(span_type(), metadata()) :: :ok

  @doc """
  Records an exception on the active span.

  Ash calls this form when the tracer has no `c:set_error/2` implementation.
  Implement at least one of the two error callbacks to handle raised exceptions.
  """
  @callback set_error(Exception.t()) :: :ok

  @optional_callbacks set_error: 2, set_error: 1, trace_type?: 1, set_handled_error: 2

  @doc """
  Runs a block inside a named span and returns the block's result.

  Pass a tracer module, a list of modules, or `nil`. Only tracers that accept
  the span type receive it. The name can be a string or a zero-arity function
  that builds it when tracing is enabled. If the block raises, Ash records the
  exception, closes the span, and reraises it.

  ## Examples

      require Ash.Tracer

      tracer = Ash.Tracer.Simple

      Ash.Tracer.span :custom, "normalize title", tracer do
        Ash.Tracer.set_metadata(tracer, :custom, %{source: :import})
        String.trim(" Draft ")
      end

      Ash.Tracer.Simple.gather_spans()

  ## See also

  - `set_metadata/3` for annotating the span
  - `telemetry_span/4` for emitting Telemetry events around a block
  """
  defmacro span(type, name, tracer, block_opts \\ []) do
    quote generated: true do
      type = unquote(type)
      name = unquote(name)

      tracer = List.wrap(unquote(tracer))
      tracer = Enum.filter(tracer, &Ash.Tracer.trace_type?(&1, type))

      # no need to use try/rescue/after if no tracers
      if Enum.empty?(tracer) do
        unquote(block_opts[:do])
      else
        Ash.Tracer.start_span(tracer, type, name)

        try do
          unquote(block_opts[:do])
        rescue
          e ->
            Ash.Tracer.set_error(tracer, e, stacktrace: __STACKTRACE__)
            reraise e, __STACKTRACE__
        after
          Ash.Tracer.stop_span(tracer)
        end
      end
    end
  end

  # `:telemetry.list_handlers/1` matches on an event *prefix*, which telemetry implements
  # as a scan over every handler in the VM. Its cost scales with the number of handlers
  # attached by all libraries, not just Ash. `telemetry_span/4` only ever emits
  # `name ++ [:start]` and `name ++ [:stop]`, so we check those two exact names with the same
  # O(1) lookup that `:telemetry.execute/3` itself uses.
  #
  # `:telemetry_handler_table.list_for_event/1` is documented as private but has been
  # exported and used by `:telemetry.execute/3` since telemetry 0.4, so we use it when
  # present and fall back to the public prefix scan otherwise.
  @fast_handler_lookup? Code.ensure_loaded?(:telemetry_handler_table) and
                          function_exported?(:telemetry_handler_table, :list_for_event, 1)

  #
  # The compilation check comes first because telemetry < 1.4.1 can crash or warn when invoked
  # before the telemetry application has started, e.g. during compile time.
  @doc false
  @spec telemetry_handlers?([atom()], [atom()]) :: boolean()
  if @fast_handler_lookup? do
    def telemetry_handlers?(start_name, stop_name) do
      not Code.can_await_module_compilation?() and
        (:telemetry_handler_table.list_for_event(start_name) != [] or
           :telemetry_handler_table.list_for_event(stop_name) != [])
    end
  else
    def telemetry_handlers?(start_name, stop_name) do
      not Code.can_await_module_compilation?() and
        (:telemetry.list_handlers(start_name) != [] or
           :telemetry.list_handlers(stop_name) != [])
    end
  end

  @doc """
  Emits Telemetry start and stop events around a block and returns its result.

  Pass an event name as a list of atoms. Ash appends `:start` and `:stop` to
  form the two event names. Both events carry `:system_time` and the stop event
  also carries `:duration`. Measurements use the VM's native time unit.

  The metadata can be a map or a zero-arity function returning a map. Ash
  evaluates metadata only when at least one of the events has a handler.
  The stop event is emitted when the block exits, including when it raises.
  Telemetry handlers receive these events independently of configured tracers.

  ## Options

  - `:skip?` runs the block without emitting events or evaluating metadata
    when true. By default, Ash checks for handlers and emits events when
    any are attached.

  ## Examples

      require Ash.Tracer

      Ash.Tracer.telemetry_span [:my_app, :import, :normalize], %{source: :feed} do
        String.trim(" Draft ")
      end

  See [Monitoring](/documentation/topics/advanced/monitoring.md#telemetry) for
  the events Ash emits.
  """
  defmacro telemetry_span(name, metadata, opts \\ [], block_opts) do
    quote generated: true do
      if unquote(opts[:skip?]) do
        unquote(block_opts[:do])
      else
        telemetry_name = unquote(name)
        start_name = telemetry_name ++ [:start]
        stop_name = telemetry_name ++ [:stop]

        # When nothing is listening, skip the clock reads, metadata construction and
        # both `:telemetry.execute/3` calls (each of which repeats the handler lookup).
        emit? = Ash.Tracer.telemetry_handlers?(start_name, stop_name)

        {metadata, start} =
          if emit? do
            metadata = unquote(metadata)
            metadata = if is_function(metadata), do: apply(metadata, []), else: metadata
            start = System.monotonic_time()
            :telemetry.execute(start_name, %{system_time: System.system_time()}, metadata)
            {metadata, start}
          else
            {nil, nil}
          end

        try do
          unquote(block_opts[:do])
        after
          if emit? do
            :telemetry.execute(
              stop_name,
              %{system_time: System.system_time(), duration: System.monotonic_time() - start},
              metadata
            )
          end
        end
      end
    end
  end

  @doc """
  Finishes the active span on each supplied tracer.

  Pass the same tracer module or list used with `start_span/3`. A `nil`
  tracer returns `:ok`. Use `span/4` to pair start and stop calls automatically.
  """
  @spec stop_span(nil | module() | [module()]) :: :ok
  def stop_span(nil), do: :ok

  def stop_span(tracers) when is_list(tracers) do
    Enum.each(tracers, &stop_span/1)
  end

  def stop_span(tracer) do
    result = apply(tracer, :stop_span, [])

    if result == :ok do
      :ok
    else
      raise Ash.Error.Framework.InvalidReturnType,
        message: """
        Invalid value returned from #{inspect(tracer)}.stop_span/0.

        The callback #{inspect(__MODULE__)}.stop_span/0 expects :ok.
        """
    end
  end

  @doc """
  Returns whether a tracer accepts the given span type.

  Delegates to `c:trace_type?/1` when implemented and defaults to `true`.
  """
  @spec trace_type?(module(), span_type()) :: boolean()
  def trace_type?(tracer, type) do
    if function_exported?(tracer, :trace_type?, 1) do
      result = apply(tracer, :trace_type?, [type])

      if is_boolean(result) do
        result
      else
        raise Ash.Error.Framework.InvalidReturnType,
          message: """
          Invalid value returned from #{inspect(tracer)}.trace_type?/1.

          The callback #{inspect(__MODULE__)}.trace_type?/1 expects a boolean.
          """
      end
    else
      true
    end
  end

  @doc """
  Starts a span on each supplied tracer.

  Pass a name or a zero-arity function that returns the name. A `nil` tracer
  leaves the name unevaluated and returns `:ok`. This function starts spans
  on the supplied modules directly. Use `span/4` for type filtering and
  automatic cleanup.
  """
  @spec start_span(nil | module() | [module()], span_type(), String.t() | (-> String.t())) :: :ok
  def start_span(nil, _type, _name), do: :ok

  def start_span(tracers, type, name) when is_list(tracers) do
    name = resolve_lazy(name)
    Enum.each(tracers, &start_span(&1, type, name))
  end

  def start_span(tracer, type, name) do
    name = resolve_lazy(name)
    result = apply(tracer, :start_span, [type, name])

    if result == :ok do
      :ok
    else
      raise Ash.Error.Framework.InvalidReturnType,
        message: """
        Invalid value returned from #{inspect(tracer)}.start_span/2.

        The callback #{inspect(__MODULE__)}.start_span/2 expects :ok.
        """
    end
  end

  @doc """
  Reports an error that was handled by the caller to each supplied tracer.

  The tracer may implement `c:set_handled_error/2` to distinguish an error
  result from a raised exception. Tracers without that callback and a `nil`
  tracer are skipped.

  ## Options

  - `:stacktrace` supplies the stacktrace, when available. The options are
    passed through to the callback.
  """
  @spec set_handled_error(nil | module() | [module()], Exception.t(), Keyword.t()) :: :ok
  def set_handled_error(nil, _, _), do: :ok

  def set_handled_error(tracers, error, opts) when is_list(tracers) do
    Enum.each(tracers, &set_handled_error(&1, error, opts))
  end

  def set_handled_error(tracer, error, opts) do
    if function_exported?(tracer, :set_handled_error, 2) do
      result = apply(tracer, :set_handled_error, [error, opts])

      if result == :ok do
        :ok
      else
        raise Ash.Error.Framework.InvalidReturnType,
          message: """
          Invalid value returned from #{inspect(tracer)}.set_handled_error/2.

          The callback #{inspect(__MODULE__)}.set_handled_error/2 expects :ok.
          """
      end
    else
      :ok
    end
  end

  @doc """
  Reports an exception to the current span on each supplied tracer.

  Calls `c:set_error/2` when available and falls back to `c:set_error/1`.
  A `nil` tracer returns `:ok`. `span/4` calls this automatically when
  its block raises.

  ## Options

  - `:stacktrace` supplies the stacktrace. The options are passed through
    when the tracer implements `c:set_error/2`.
  """
  @spec set_error(nil | module() | [module()], Exception.t(), Keyword.t()) :: :ok
  def set_error(nil, _, _), do: :ok

  def set_error(tracers, error, opts) when is_list(tracers) do
    Enum.each(tracers, &set_error(&1, error, opts))
  end

  def set_error(tracer, error, opts) do
    result =
      if function_exported?(tracer, :set_error, 2) do
        apply(tracer, :set_error, [error, opts])
      else
        apply(tracer, :set_error, [error])
      end

    if result == :ok do
      :ok
    else
      raise Ash.Error.Framework.InvalidReturnType,
        message: """
        Invalid value returned from #{inspect(tracer)}.set_error/1 or set_error/2.

        The callback #{inspect(__MODULE__)}.set_error expects :ok.
        """
    end
  end

  @doc """
  Reports an exception to a tracer without extra options.

  Equivalent to `set_error(tracer, error, [])`. The tracer argument can be
  a single module, a list of modules, or `nil`.
  """
  @spec set_error(nil | module() | [module()], Exception.t()) :: :ok
  def set_error(nil, _), do: :ok

  def set_error(tracers, error) when is_list(tracers) do
    Enum.each(tracers, &set_error(&1, error))
  end

  def set_error(tracer, error) do
    result =
      if function_exported?(tracer, :set_error, 2) do
        apply(tracer, :set_error, [error, []])
      else
        apply(tracer, :set_error, [error])
      end

    if result == :ok do
      :ok
    else
      raise Ash.Error.Framework.InvalidReturnType,
        message: """
        Invalid value returned from #{inspect(tracer)}.set_error/1 or set_error/2.

        The callback #{inspect(__MODULE__)}.set_error expects :ok.
        """
    end
  end

  @doc """
  Captures a single tracer's context in the calling process.

  Pass the returned value to `set_span_context/2` in a worker process.
  This function accepts one tracer because each tracer defines its own context.
  To transfer context for a list of tracers, use
  `Ash.ProcessHelpers.get_context_for_transfer/1` with the `:tracer` option.
  """
  @spec get_span_context(nil | module()) :: term()
  def get_span_context(nil), do: :ok

  def get_span_context(tracer) when is_list(tracer) do
    raise ArgumentError, "Cannot get span context from multiple tracers"
  end

  def get_span_context(tracer) do
    apply(tracer, :get_span_context, [])
  end

  @doc """
  Installs a single tracer's context in the calling process.

  Capture the context with `get_span_context/1` before starting a worker,
  then install it in the worker before opening spans there. A `nil` tracer
  returns `:ok`.
  """
  @spec set_span_context(nil | module(), term()) :: :ok
  def set_span_context(nil, _), do: :ok

  def set_span_context(tracer, _context) when is_list(tracer) do
    raise ArgumentError, "Cannot set span context from multiple tracers"
  end

  def set_span_context(tracer, context) do
    result = apply(tracer, :set_span_context, [context])

    if result == :ok do
      :ok
    else
      raise Ash.Error.Framework.InvalidReturnType,
        message: """
        Invalid value returned from #{inspect(tracer)}.set_span_context/1.

        The callback #{inspect(__MODULE__)}.set_span_context/1 expects :ok.
        """
    end
  end

  @doc """
  Adds metadata to the active span on each tracer that accepts the span type.

  Call this inside a `span/4` block with the same tracer and type. Pass a map
  or a zero-arity function returning a map. The function is evaluated only if
  at least one supplied tracer accepts the type.

      Ash.Tracer.set_metadata(tracer, :custom, fn -> %{source: :import} end)
  """
  @spec set_metadata(nil | module() | [module()], span_type(), metadata() | (-> metadata())) ::
          :ok
  def set_metadata(nil, _type, _metadata), do: :ok

  def set_metadata(tracers, type, metadata) when is_list(tracers) do
    tracers = Enum.filter(tracers, &Ash.Tracer.trace_type?(&1, type))

    if !Enum.empty?(tracers) do
      metadata = resolve_lazy(metadata)
      Enum.each(tracers, &set_metadata(&1, type, metadata))
    end
  end

  def set_metadata(tracer, type, metadata) do
    if Ash.Tracer.trace_type?(tracer, type) do
      metadata = resolve_lazy(metadata)
      result = apply(tracer, :set_metadata, [type, metadata])

      if result == :ok do
        :ok
      else
        raise Ash.Error.Framework.InvalidReturnType,
          message: """
          Invalid value returned from #{inspect(tracer)}.set_metadata/2.

          The callback #{inspect(__MODULE__)}.set_metadata/2 expects :ok.
          """
      end
    end
  end

  @doc """
  Declares a module as an implementation of the `Ash.Tracer` behaviour.

  The macro takes no configuration options. Implement the span lifecycle,
  metadata, and context callbacks, plus one of the exception callbacks.
  Use `c:trace_type?/1` to select the spans your tracer handles. For example,
  this tracer collects action spans using `Ash.Tracer.Simple`:

      defmodule MyApp.ActionTracer do
        use Ash.Tracer

        @impl true
        def trace_type?(type), do: type == :action

        @impl true
        defdelegate start_span(type, name), to: Ash.Tracer.Simple
        @impl true
        defdelegate stop_span(), to: Ash.Tracer.Simple
        @impl true
        defdelegate set_metadata(type, metadata), to: Ash.Tracer.Simple
        @impl true
        defdelegate get_span_context(), to: Ash.Tracer.Simple
        @impl true
        defdelegate set_span_context(context), to: Ash.Tracer.Simple
        @impl true
        defdelegate set_error(error), to: Ash.Tracer.Simple
      end

  ## Configuring a tracer

  Pass the module as the `:tracer` option to an action:

      Ash.read!(MyApp.Post, tracer: MyApp.ActionTracer)
      Ash.Tracer.Simple.gather_spans()

  To trace setup work as well, pass `:tracer` when building the query,
  changeset, or action input. To trace across your application, put
  `config :ash, :tracer, MyApp.Tracer` in your application config. The option
  and the application config both accept a module or a list of modules.

  See [Monitoring](/documentation/topics/advanced/monitoring.md#tracing) for
  Ash span types and naming configuration.
  """
  defmacro __using__(_) do
    quote do
      @behaviour Ash.Tracer
    end
  end

  defp resolve_lazy(value) do
    if is_function(value), do: apply(value, []), else: value
  end
end
