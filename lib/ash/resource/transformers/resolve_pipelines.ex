# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Transformers.ResolvePipelines do
  @moduledoc "Resolves `pipe_through` on actions by injecting pipeline changes/preparations."
  use Spark.Dsl.Transformer

  alias Spark.Dsl.Transformer
  alias Spark.Error.DslError

  def before?(Ash.Resource.Transformers.RequireUniqueActionNames), do: true
  def before?(Ash.Resource.Transformers.SetPrimaryActions), do: true
  def before?(Ash.Resource.Transformers.DefaultAccept), do: true
  def before?(Ash.Resource.Transformers.GetByReadActions), do: true
  def before?(_), do: false

  def transform(dsl_state) do
    pipelines =
      dsl_state
      |> Transformer.get_entities([:pipelines])
      |> Map.new(&{&1.name, &1})

    dsl_state
    |> Transformer.get_entities([:actions])
    |> Enum.filter(&(&1.pipe_through != []))
    |> Enum.reduce_while({:ok, dsl_state}, fn action, {:ok, dsl_state} ->
      case resolve_pipe_through(action, pipelines, dsl_state) do
        {:ok, updated_action} ->
          new_state =
            Transformer.replace_entity(
              dsl_state,
              [:actions],
              updated_action,
              &(&1.name == action.name && &1.type == action.type)
            )

          {:cont, {:ok, new_state}}

        {:error, error} ->
          {:halt, {:error, error}}
      end
    end)
  end

  defp resolve_pipe_through(action, pipelines, dsl_state) do
    # Flatten all pipe_through declarations into {name, where_conditions} pairs
    pairs =
      Enum.flat_map(action.pipe_through, fn %{names: names, where: where} ->
        Enum.map(names, &{&1, where})
      end)

    pairs
    |> Enum.reduce_while({:ok, {[], [], []}}, fn {name, where},
                                                 {:ok, {entities, arguments, accept}} ->
      case Map.fetch(pipelines, name) do
        {:ok, pipeline} ->
          new_entities = collect_entities(action.type, pipeline, where)

          {:cont,
           {:ok,
            {[new_entities | entities], [pipeline.arguments | arguments],
             [pipeline.accept | accept]}}}

        :error ->
          {:halt,
           {:error,
            DslError.exception(
              module: Transformer.get_persisted(dsl_state, :module),
              path: [:actions, action.type],
              message:
                "Action `#{action.name}` references pipeline `#{name}` via `pipe_through`, but no pipeline named `#{name}` exists."
            )}}
      end
    end)
    |> case do
      {:ok, {entities, arguments, accept}} ->
        {:ok,
         apply_to_action(
           action,
           entities |> Enum.reverse() |> Enum.concat(),
           arguments |> Enum.reverse() |> Enum.concat(),
           accept |> Enum.reverse() |> Enum.concat(),
           dsl_state
         )}

      {:error, error} ->
        {:error, error}
    end
  end

  defp apply_to_action(action, entities, arguments, accept, dsl_state) do
    action
    |> prepend_entities(entities)
    |> merge_arguments(arguments, dsl_state)
    |> merge_accept(accept)
  end

  defp prepend_entities(action, entities) do
    field = entity_field(action.type)
    Map.update!(action, field, &(entities ++ &1))
  end

  defp merge_arguments(action, [], _dsl_state), do: action

  defp merge_arguments(action, pipeline_arguments, dsl_state) do
    action_args_by_name = Map.new(action.arguments, &{&1.name, &1})

    {new_args, conflicts} =
      Enum.reduce(pipeline_arguments, {[], []}, fn pipeline_arg, {keep, conflicts} ->
        case Map.fetch(action_args_by_name, pipeline_arg.name) do
          :error ->
            {[pipeline_arg | keep], conflicts}

          {:ok, action_arg} ->
            if action_arg.type == pipeline_arg.type do
              # same type — action's version wins, skip pipeline's
              {keep, conflicts}
            else
              {keep, [pipeline_arg | conflicts]}
            end
        end
      end)

    if conflicts != [] do
      conflict_details =
        Enum.map_join(conflicts, ", ", fn arg ->
          action_arg = action_args_by_name[arg.name]
          "`#{arg.name}` (pipeline: #{inspect(arg.type)}, action: #{inspect(action_arg.type)})"
        end)

      raise DslError,
        module: Transformer.get_persisted(dsl_state, :module),
        path: [:actions, action.type, action.name],
        message:
          "Action `#{action.name}` redefines pipeline argument(s) with a different type: #{conflict_details}"
    end

    Map.update!(action, :arguments, &(Enum.reverse(new_args) ++ &1))
  end

  defp merge_accept(action, []), do: action

  defp merge_accept(action, accept) do
    case Map.get(action, :accept) do
      nil ->
        Map.put(action, :accept, accept)

      :* ->
        action

      existing when is_list(existing) ->
        Map.put(action, :accept, Enum.uniq(accept ++ existing))
    end
  end

  defp collect_entities(type, pipeline, where) do
    source_entities(type, pipeline)
    |> Enum.map(fn entity ->
      Map.update!(entity, :where, &(where ++ &1))
    end)
  end

  defp source_entities(type, pipeline) when type in [:create, :update, :destroy],
    do: pipeline.changes ++ pipeline.validations

  defp source_entities(type, pipeline) when type in [:read, :action],
    do: pipeline.preparations ++ pipeline.validations

  defp entity_field(type) when type in [:create, :update, :destroy], do: :changes
  defp entity_field(type) when type in [:read, :action], do: :preparations
end
