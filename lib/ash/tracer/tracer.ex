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
          | {:custom, atom()}

  @type metadata() :: %{
          api: nil | module(),
          resource: nil | module(),
          actor: term(),
          tenant: nil | String.t(),
          action: atom(),
          authorize?: boolean()
        }

  @callback start_span(span_type(), name :: String.t()) :: :ok
  @callback stop_span() :: :ok
  @callback get_span_context() :: term()
  @callback set_span_context(term()) :: :ok

  @doc """
  Set metadata for the current span.

  This may be called multiple times per span, and should ideally merge with previous metadata.
  """
  @callback set_metadata(span_type(), metadata()) :: :ok
  @callback set_error(Exception.t()) :: :ok

  defmacro span(type, name, tracer, block_opts \\ []) do
    quote do
      type = unquote(type)
      name = unquote(name)
      tracer = unquote(tracer)

      if tracer do
        tracer.start_span(type, name)

        try do
          unquote(block_opts[:do])
        rescue
          e ->
            tracer.set_error(e)
            reraise e, __STACKTRACE__
        after
          tracer.stop_span()
        end
      else
        unquote(block_opts[:do])
      end
    end
  end

  defmacro telemetry_span(name, metadata, opts) do
    quote do
      telemetry_name = unquote(name)
      metadata = unquote(metadata)

      start = System.monotonic_time()

      :telemetry.execute(
        telemetry_name ++ [:start],
        %{system_time: System.system_time()},
        metadata
      )

      try do
        unquote(opts[:do])
      after
        duration = System.monotonic_time() - start

        :telemetry.execute(
          telemetry_name ++ [:stop],
          %{system_time: System.system_time(), duration: duration},
          metadata
        )
      end
    end
  end

  def set_metadata(nil, _type, _metadata), do: :ok

  def set_metadata(tracer, type, metadata) do
    tracer.set_metadata(type, metadata)
  end

  defmacro __using__(_) do
    quote do
      @behaviour Ash.Tracer
    end
  end
end
