# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Signals.Delivery do
  @moduledoc """
  A behaviour for delivering signals to their listeners.

  Each listener declares how signals reach it, with its `delivery` option. The default,
  `Ash.Signals.Delivery.Inline`, runs the listener immediately, during the phase in which the
  signal is emitted. Other deliveries can, for example, enqueue a job, so that the listener runs
  after the emitting action's transaction commits.

  ```elixir
  signals_in do
    from MyApp.Shop.Signals do
      on :order_placed, :send_receipt, delivery: {MyApp.JobDelivery, queue: :mailers}
    end
  end
  ```

  When a signal is emitted, during the signal's phase and after every payload has been cast,
  `c:deliver/4` is called once for each delivery and options its listeners use, with those
  listeners. To run a listener, immediately or later, use `Ash.Signals.run_listener/3`, which does
  exactly what the inline delivery does. A delivery that runs listeners later can store signals
  with `Ash.Signals.dump/3`, load them with `Ash.Signals.load/3`, and find the listener again
  with `Ash.Resource.Info.signal_listener/2`.
  """

  defmodule Context do
    @moduledoc """
    The context in which a signal was emitted, passed to `c:Ash.Signals.Delivery.deliver/4`.

    It implements `Ash.Scope.ToOpts`, so `Ash.Scope.to_opts(context)` returns the options to run
    listeners with, including the `signal_path` in their context.

    `signal_path` starts with this signal, followed by the signals that led to it. A delivery that runs listeners later must store it, and pass it back in the
    `signal_path` key of the context. See `t:Ash.Signals.signal_path/0`.
    """
    defstruct [
      :signal_module,
      :signal,
      :resource,
      :action,
      :actor,
      :tenant,
      :authorize?,
      :tracer,
      source_context: %{},
      signal_path: []
    ]

    @type t :: %__MODULE__{
            signal_module: Ash.Signals.t(),
            signal: Ash.Signals.Signal.t(),
            resource: Ash.Resource.t(),
            action: atom() | nil,
            actor: term(),
            tenant: term(),
            authorize?: boolean() | nil,
            tracer: Ash.Tracer.t() | [Ash.Tracer.t()] | nil,
            source_context: map(),
            signal_path: Ash.Signals.signal_path()
          }

    defimpl Ash.Scope.ToOpts do
      def get_actor(%{actor: actor}), do: {:ok, actor}
      def get_tenant(%{tenant: tenant}), do: {:ok, tenant}

      def get_context(%{source_context: source_context, signal_path: signal_path}),
        do: {:ok, Map.put(source_context, :signal_path, signal_path)}

      def get_tracer(%{tracer: tracer}), do: {:ok, tracer}
      def get_authorize?(%{authorize?: authorize?}), do: {:ok, authorize?}
    end
  end

  @doc """
  Delivers signals to the given listeners.

  `signals` are the signal structs emitted together, `listeners` are the listeners of the signal
  that use this delivery with these options, and `opts` are the options given to the delivery in
  their `delivery` option. Returning an error fails the emit.
  """
  @callback deliver(
              signals :: [struct()],
              listeners :: [Ash.Resource.SignalListener.t()],
              opts :: Keyword.t(),
              context :: Context.t()
            ) :: :ok | {:error, term()}

  @doc """
  Verifies a listener that uses this delivery, when the listening resource is compiled. Receives
  the signal it listens to, and the options given to the delivery.
  """
  @callback verify_listener(
              listener :: Ash.Resource.SignalListener.t(),
              signal :: Ash.Signals.Signal.t(),
              opts :: Keyword.t(),
              dsl_state :: Spark.Dsl.t()
            ) :: :ok | {:error, String.t()}

  @optional_callbacks verify_listener: 4
end
