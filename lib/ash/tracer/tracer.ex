# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Tracer do
  @moduledoc """
  A behaviour for implementing tracing for an Ash application.
  """

  @type span_type() ::
          :action
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

  defmacro telemetry_span(name, metadata, opts \\ [], block_opts) do
    quote generated: true do
      if unquote(opts[:skip?]) do
        unquote(block_opts[:do])
      else
        telemetry_name = unquote(name)

        metadata = unquote(metadata)

        compiling? = Code.can_await_module_compilation?()

        metadata =
          if !compiling? do
            case :telemetry.list_handlers(telemetry_name) do
              [] -> %{}
              _ when is_function(metadata) -> apply(metadata, [])
              _ -> metadata
            end
          end

        start = System.monotonic_time()

        if !compiling? do
          :telemetry.execute(
            telemetry_name ++ [:start],
            %{system_time: System.system_time()},
            metadata
          )
        end

        try do
          unquote(block_opts[:do])
        after
          duration = System.monotonic_time() - start

          if !compiling? do
            :telemetry.execute(
              telemetry_name ++ [:stop],
              %{system_time: System.system_time(), duration: duration},
              metadata
            )
          end
        end
      end
    end
  end

  @spec stop_span(nil | module() | [module()]) :: :ok
  def stop_span(nil), do: :ok

  def stop_span(tracers) when is_list(tracers) do
    Enum.each(tracers, &stop_span/1)
  end

  def stop_span(tracer) do
    result = tracer.stop_span()

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

  @spec trace_type?(module(), atom()) :: boolean()
  def trace_type?(tracer, type) do
    if function_exported?(tracer, :trace_type?, 1) do
      result = tracer.trace_type?(type)

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

  @spec start_span(nil | module() | [module()], span_type(), String.t() | (-> String.t())) :: :ok
  def start_span(nil, _type, _name), do: :ok

  def start_span(tracers, type, name) when is_list(tracers) do
    name = resolve_lazy(name)
    Enum.each(tracers, &start_span(&1, type, name))
  end

  def start_span(tracer, type, name) do
    name = resolve_lazy(name)
    result = tracer.start_span(type, name)

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

  @spec set_handled_error(nil | module() | [module()], Exception.t(), Keyword.t()) :: :ok
  def set_handled_error(nil, _, _), do: :ok

  def set_handled_error(tracers, error, opts) when is_list(tracers) do
    Enum.each(tracers, &set_handled_error(&1, error, opts))
  end

  def set_handled_error(tracer, error, opts) do
    if function_exported?(tracer, :set_handled_error, 2) do
      result = tracer.set_handled_error(error, opts)

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

  @spec set_error(nil | module() | [module()], Exception.t(), Keyword.t()) :: :ok
  def set_error(nil, _, _), do: :ok

  def set_error(tracers, error, opts) when is_list(tracers) do
    Enum.each(tracers, &set_error(&1, error, opts))
  end

  def set_error(tracer, error, opts) do
    result =
      if function_exported?(tracer, :set_error, 2) do
        tracer.set_error(error, opts)
      else
        tracer.set_error(error)
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

  @spec set_error(nil | module() | [module()], Exception.t()) :: :ok
  def set_error(nil, _), do: :ok

  def set_error(tracers, error) when is_list(tracers) do
    Enum.each(tracers, &set_error(&1, error))
  end

  def set_error(tracer, error) do
    result =
      if function_exported?(tracer, :set_error, 2) do
        tracer.set_error(error, [])
      else
        tracer.set_error(error)
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

  @spec get_span_context(nil | module()) :: term()
  def get_span_context(nil), do: :ok

  def get_span_context(tracer) when is_list(tracer) do
    raise ArgumentError, "Cannot get span context from multiple tracers"
  end

  def get_span_context(tracer) do
    tracer.get_span_context()
  end

  @spec set_span_context(nil | module(), term()) :: :ok
  def set_span_context(nil, _), do: :ok

  def set_span_context(tracer, _context) when is_list(tracer) do
    raise ArgumentError, "Cannot set span context from multiple tracers"
  end

  def set_span_context(tracer, context) do
    result = tracer.set_span_context(context)

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
      result = tracer.set_metadata(type, metadata)

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

  defmacro __using__(_) do
    quote do
      @behaviour Ash.Tracer
    end
  end

  defp resolve_lazy(value) do
    if is_function(value), do: apply(value, []), else: value
  end
end
