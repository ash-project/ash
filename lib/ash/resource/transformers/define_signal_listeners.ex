# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Transformers.DefineSignalListeners do
  @moduledoc false
  # Flattens the listeners in each `from`, resolves their resource, signal module, action and
  # defaults, persists them as `:signal_listeners`, and defines an
  # `Ash.Signals.Listeners.<Resource>` module returning them, which is how `Ash.Signals.Registry`
  # finds listeners.
  use Spark.Dsl.Transformer

  alias Spark.Dsl.Transformer

  def transform(dsl_state) do
    resource = Transformer.get_persisted(dsl_state, :module)

    listeners =
      for source <- Transformer.get_entities(dsl_state, [:signals_in]),
          listener <- source.listeners do
        resolve(%{listener | signal_module: source.signal_module}, resource, dsl_state)
      end

    dsl_state = Transformer.persist(dsl_state, :signal_listeners, listeners)

    case listeners do
      [] ->
        {:ok, dsl_state}

      listeners ->
        listeners = Enum.map(listeners, &%{&1 | __spark_metadata__: nil})
        {:ok, Transformer.eval(dsl_state, [], define_listener_module(resource, listeners))}
    end
  end

  defp resolve(listener, resource, dsl_state) do
    listener = %{listener | action: listener.action || listener.name}

    action_type =
      case Ash.Resource.Info.action(dsl_state, listener.action) do
        %{type: type} -> type
        nil -> nil
      end

    # options that don't apply to the action type are kept, so the verifier can report them
    argument =
      cond do
        action_type != :action -> listener.argument
        listener.argument -> listener.argument
        listener.batch? -> :signals
        true -> :signal
      end

    read_action =
      if action_type in [:update, :destroy] do
        listener.read_action ||
          case Ash.Resource.Info.primary_action(dsl_state, :read) do
            %{name: name} -> name
            nil -> nil
          end
      else
        listener.read_action
      end

    %{
      listener
      | resource: resource,
        action_type: action_type,
        argument: argument,
        read_action: read_action,
        inputs: Ash.Resource.SignalListener.normalize_mapping(listener.inputs),
        args: Ash.Resource.SignalListener.normalize_mapping(listener.args),
        get_by: Ash.Resource.SignalListener.normalize_mapping(listener.get_by)
    }
  end

  defp define_listener_module(resource, listeners) do
    body =
      quote do
        @moduledoc false

        @doc false
        def listeners, do: unquote(Macro.escape(listeners))
      end

    quote do
      Module.create(
        unquote(Ash.Signals.Registry.listener_module(resource)),
        unquote(Macro.escape(body)),
        Macro.Env.location(__ENV__)
      )
    end
  end
end
