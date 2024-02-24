defmodule Ash.Tracer do
  @moduledoc """
  A behaviour for implementing tracing for an Ash application.
  """

  @type span_type() ::
          :action
          | :changeset
          | :query
          | :flow
          | :request_step
          | :change
          | :validation
          | :preparation
          | :custom_flow_step
          | :custom
          | :before_transaction
          | :before_action
          | :after_transaction
          | :after_action
          | {:custom, atom()}

  @type metadata() :: %{
          domain: nil | module(),
          resource: nil | module(),
          actor: term(),
          tenant: term(),
          action: atom(),
          authorize?: boolean()
        }

  @type t :: module

  @callback start_span(span_type(), name :: String.t()) :: :ok
  @callback stop_span() :: :ok
  @callback get_span_context() :: term()
  @callback set_span_context(term()) :: :ok
  @callback set_error(Exception.t(), Keyword.t()) :: :ok
  @callback trace_type?(atom) :: boolean()
  @callback set_handled_error(Exception.t(), Keyword.t()) :: :ok

  @doc """
  Set metadata for the current span.

  This may be called multiple times per span, and should ideally merge with previous metadata.
  """
  @callback set_metadata(span_type(), metadata()) :: :ok
  @callback set_error(Exception.t()) :: :ok

  @optional_callbacks set_error: 2, set_error: 1, trace_type?: 1, set_handled_error: 2

  defmacro span(type, name, tracer, block_opts \\ []) do
    quote do
      type = unquote(type)
      name = unquote(name)
      tracer = List.wrap(unquote(tracer))
      tracer = Enum.filter(tracer, &Ash.Tracer.trace_type?(&1, type))

      Ash.Tracer.start_span(tracer, type, name)

      # no need to use try/rescue/after if no tracers
      if Enum.empty?(tracer) do
        unquote(block_opts[:do])
      else
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

  defmacro telemetry_span(name, metadata, opts) do
    quote do
      telemetry_name = unquote(name)
      metadata = unquote(metadata)

      start = System.monotonic_time()

      compiling? = Code.can_await_module_compilation?()

      unless compiling? do
        :telemetry.execute(
          telemetry_name ++ [:start],
          %{system_time: System.system_time()},
          metadata
        )
      end

      try do
        unquote(opts[:do])
      after
        duration = System.monotonic_time() - start

        unless compiling? do
          :telemetry.execute(
            telemetry_name ++ [:stop],
            %{system_time: System.system_time(), duration: duration},
            metadata
          )
        end
      end
    end
  end

  def stop_span(nil), do: :ok

  def stop_span(tracers) when is_list(tracers) do
    Enum.each(tracers, &stop_span/1)
  end

  def stop_span(tracer) do
    tracer.stop_span()
  end

  def trace_type?(tracer, type) do
    if function_exported?(tracer, :trace_type?, 1) do
      tracer.trace_type?(type)
    else
      true
    end
  end

  def start_span(nil, _type, _name), do: :ok

  def start_span(tracers, type, name) when is_list(tracers) do
    Enum.each(tracers, &start_span(&1, type, name))
  end

  def start_span(tracer, type, name) do
    tracer.start_span(type, name)
  end

  def set_handled_error(nil, _, _), do: :ok

  def set_handled_error(tracers, error, opts) when is_list(tracers) do
    Enum.each(tracers, &set_handled_error(&1, error, opts))
  end

  def set_handled_error(tracer, error, opts) do
    if function_exported?(tracer, :set_handled_error, 2) do
      tracer.set_handled_error(error, opts)
    else
      :ok
    end
  end

  def set_error(nil, _, _), do: :ok

  def set_error(tracers, error, opts) when is_list(tracers) do
    Enum.each(tracers, &set_error(&1, error, opts))
  end

  def set_error(tracer, error, opts) do
    if function_exported?(tracer, :set_error, 2) do
      tracer.set_error(error, opts)
    else
      tracer.set_error(error)
    end
  end

  def set_error(nil, _), do: :ok

  def set_error(tracers, error) when is_list(tracers) do
    Enum.each(tracers, &set_error(&1, error))
  end

  def set_error(tracer, error) do
    if function_exported?(tracer, :set_error, 2) do
      tracer.set_error(error, [])
    else
      tracer.set_error(error)
    end
  end

  def get_span_context(nil), do: :ok

  def get_span_context(tracer) when is_list(tracer) do
    raise ArgumentError, "Cannot get span context from multiple tracers"
  end

  def get_span_context(tracer) do
    tracer.get_span_context()
  end

  def set_span_context(nil, _), do: :ok

  def set_span_context(tracer, _context) when is_list(tracer) do
    raise ArgumentError, "Cannot set span context from multiple tracers"
  end

  def set_span_context(tracer, context) do
    tracer.set_span_context(context)
  end

  def set_metadata(nil, _type, _metadata), do: :ok

  def set_metadata(tracers, type, metadata) when is_list(tracers) do
    Enum.each(tracers, &set_metadata(&1, type, metadata))
  end

  def set_metadata(tracer, type, metadata) do
    tracer.set_metadata(type, metadata)
  end

  defmacro __using__(_) do
    quote do
      @behaviour Ash.Tracer
    end
  end
end
