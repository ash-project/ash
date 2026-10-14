# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Transformers.ResolveEmits do
  @moduledoc """
  Resolves `emit` on actions: each one is replaced in place by the change (or, in generic actions,
  the preparation) that emits the signal, unless it is `dynamic?`, and every one is kept in the
  action's `emits`.
  """
  use Spark.Dsl.Transformer

  alias Ash.Resource.Actions.Emit
  alias Spark.Dsl.Transformer

  def before?(Ash.Resource.Transformers.RequireUniqueActionNames), do: true
  def before?(Ash.Resource.Transformers.SetPrimaryActions), do: true
  def before?(Ash.Resource.Transformers.DefaultAccept), do: true
  def before?(_), do: false

  def transform(dsl_state) do
    dsl_state
    |> Transformer.get_entities([:actions])
    |> Enum.reduce({:ok, dsl_state}, fn action, {:ok, dsl_state} ->
      field = if action.type == :action, do: :preparations, else: :changes
      entities = Map.get(action, field, [])

      case Enum.filter(entities, &match?(%Emit{}, &1)) do
        [] ->
          {:ok, dsl_state}

        emits ->
          resolved =
            Enum.flat_map(entities, fn
              %Emit{dynamic?: true} -> []
              %Emit{} = emit -> [emitter(action.type, emit)]
              entity -> [entity]
            end)

          {:ok,
           Transformer.replace_entity(
             dsl_state,
             [:actions],
             action |> Map.put(field, resolved) |> Map.put(:emits, emits),
             &(&1.name == action.name && &1.type == action.type)
           )}
      end
    end)
  end

  defp emitter(:action, emit) do
    %Ash.Resource.Preparation{
      preparation: {Ash.Resource.Preparation.EmitSignal, Emit.emitter_opts(emit)},
      where: emit.where,
      __spark_metadata__: emit.__spark_metadata__
    }
  end

  defp emitter(_type, emit) do
    %Ash.Resource.Change{
      change: {Ash.Resource.Change.EmitSignal, Emit.emitter_opts(emit)},
      where: emit.where,
      description: emit.description,
      only_when_valid?: false,
      always_atomic?: false,
      __spark_metadata__: emit.__spark_metadata__
    }
  end
end
