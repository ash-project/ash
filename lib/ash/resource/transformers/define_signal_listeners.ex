# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Transformers.DefineSignalListeners do
  @moduledoc false
  # Resolves each listener's resource, action type and defaults, and defines an
  # `Ash.Signals.Listeners.<Resource>` module returning them, which is how `Ash.Signals.Registry`
  # finds listeners.
  use Spark.Dsl.Transformer

  alias Spark.Dsl.Transformer

  def transform(dsl_state) do
    case Transformer.get_entities(dsl_state, [:signals_in]) do
      [] ->
        {:ok, dsl_state}

      listeners ->
        resource = Transformer.get_persisted(dsl_state, :module)

        dsl_state =
          Enum.reduce(listeners, dsl_state, fn listener, dsl_state ->
            Transformer.replace_entity(
              dsl_state,
              [:signals_in],
              resolve(listener, resource, dsl_state),
              &(&1 == listener)
            )
          end)

        listeners =
          dsl_state
          |> Transformer.get_entities([:signals_in])
          |> Enum.map(&%{&1 | __spark_metadata__: nil})

        {:ok, Transformer.eval(dsl_state, [], define_listener_module(resource, listeners))}
    end
  end

  defp resolve(listener, resource, dsl_state) do
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
