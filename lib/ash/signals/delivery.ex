# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Signals.Delivery do
  @moduledoc """
  A behaviour for delivering signals to their listeners.

  Each signal declares how it is delivered, with its `delivery` option. The default,
  `Ash.Signals.Delivery.Inline`, runs the listeners immediately, during the phase in which the
  signal is emitted. Other deliveries can, for example, enqueue a job for each listener, so that
  listeners run after the emitting action's transaction commits.

  ```elixir
  signal :order_placed do
    phase :after_action
    delivery {MyApp.JobDelivery, queue: :orders}
    argument :order_id, :uuid, allow_nil?: false
  end
  ```

  `c:deliver/4` is called each time the signal is emitted, during the signal's phase, after every
  payload has been cast. It is called even when the signal has no listeners. To run a listener,
  immediately or later, use `Ash.Signals.run_listener/3`, which does exactly what the inline
  delivery does. A delivery that runs listeners later can store signals with `Ash.Signals.dump/3`,
  load them with `Ash.Signals.load/3`, and find the listener again with
  `Ash.Resource.Info.signal_listener/2`.
  """

  defmodule Context do
    @moduledoc """
    The context in which a signal was emitted, passed to `c:Ash.Signals.Delivery.deliver/4`.

    It implements `Ash.Scope.ToOpts`, so `Ash.Scope.to_opts(context)` returns the options to run
    listeners with.
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
      source_context: %{}
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
            source_context: map()
          }

    defimpl Ash.Scope.ToOpts do
      def get_actor(%{actor: actor}), do: {:ok, actor}
      def get_tenant(%{tenant: tenant}), do: {:ok, tenant}
      def get_context(%{source_context: source_context}), do: {:ok, source_context}
      def get_tracer(%{tracer: tracer}), do: {:ok, tracer}
      def get_authorize?(%{authorize?: authorize?}), do: {:ok, authorize?}
    end
  end

  @doc """
  Delivers signals to the given listeners.

  `signals` are the signal structs emitted together, and `opts` are the options given to the
  delivery in the signal's `delivery` option. Returning an error fails the emit.
  """
  @callback deliver(
              signals :: [struct()],
              listeners :: [Ash.Resource.SignalListener.t()],
              opts :: Keyword.t(),
              context :: Context.t()
            ) :: :ok | {:error, term()}

  @doc """
  Verifies a signal that uses this delivery, when its signal module is compiled.
  """
  @callback verify_signal(
              signal :: Ash.Signals.Signal.t(),
              opts :: Keyword.t(),
              dsl_state :: Spark.Dsl.t()
            ) :: :ok | {:error, String.t()}

  @doc """
  Verifies a listener of a signal that uses this delivery, when the listening resource is
  compiled.
  """
  @callback verify_listener(
              listener :: Ash.Resource.SignalListener.t(),
              signal :: Ash.Signals.Signal.t(),
              opts :: Keyword.t(),
              dsl_state :: Spark.Dsl.t()
            ) :: :ok | {:error, String.t()}

  @optional_callbacks verify_signal: 3, verify_listener: 4
end
