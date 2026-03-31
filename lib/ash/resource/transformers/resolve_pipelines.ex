# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Transformers.ResolvePipelines do
  @moduledoc "Resolves `pipe_through` on actions by replacing PipeThrough entities with pipeline contents in-place."
  use Spark.Dsl.Transformer

  alias Ash.Resource.Actions.PipeThrough
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
    |> Enum.reduce({:ok, dsl_state}, fn action, {:ok, dsl_state} ->
      field = entity_field(action.type)
      entities = Map.get(action, field, [])

      if Enum.any?(entities, &match?(%PipeThrough{}, &1)) do
        expanded = resolve_action(action, entities, pipelines, dsl_state)

        new_state =
          Transformer.replace_entity(
            dsl_state,
            [:actions],
            Map.put(action, field, expanded),
            &(&1.name == action.name && &1.type == action.type)
          )

        {:ok, new_state}
      else
        {:ok, dsl_state}
      end
    end)
  end

  defp resolve_action(action, entities, pipelines, dsl_state) do
    Enum.flat_map(entities, fn
      %PipeThrough{names: names, where: where} ->
        expand_pipe_through(names, where, action, pipelines, dsl_state)

      entity ->
        [entity]
    end)
  end

  defp expand_pipe_through(names, where, action, pipelines, dsl_state) do
    Enum.flat_map(names, fn name ->
      case Map.fetch(pipelines, name) do
        {:ok, pipeline} ->
          collect_entities(action.type, pipeline, where, dsl_state)

        :error ->
          raise DslError,
            module: Transformer.get_persisted(dsl_state, :module),
            path: [:actions, action.type],
            message:
              "Action `#{action.name}` references pipeline `#{name}` via `pipe_through`, but no pipeline named `#{name}` exists."
      end
    end)
  end

  defp collect_entities(type, pipeline, where, dsl_state) do
    subject = subject_for_type(type)

    source_entities(pipeline, type)
    |> Enum.map(fn entity ->
      validate_supports!(entity, subject, pipeline, dsl_state)
      Map.update!(entity, :where, &(where ++ &1))
    end)
  end

  defp validate_supports!(entity, subject, pipeline, dsl_state) do
    case entity_module_and_opts(entity) do
      {module, opts} ->
        supported = module.supports(opts)

        if subject not in supported do
          label = entity.__struct__ |> Module.split() |> List.last()

          raise DslError,
            module: Transformer.get_persisted(dsl_state, :module),
            path: [:pipelines, pipeline.name],
            message:
              "#{label} `#{inspect(module)}` in pipeline `#{pipeline.name}` does not support `#{inspect(subject)}` (supported: #{inspect(supported)})"
        end

      nil ->
        :ok
    end
  end

  defp entity_module_and_opts(%Ash.Resource.Validation{module: module, opts: opts}),
    do: {module, opts}

  defp entity_module_and_opts(%Ash.Resource.Preparation{preparation: {module, opts}}),
    do: {module, opts}

  defp entity_module_and_opts(_), do: nil

  defp source_entities(pipeline, type) when type in [:create, :update, :destroy],
    do: pipeline.changes ++ pipeline.validations

  defp source_entities(pipeline, type) when type in [:read, :action],
    do: pipeline.preparations ++ pipeline.validations

  defp entity_field(type) when type in [:create, :update, :destroy], do: :changes
  defp entity_field(type) when type in [:read, :action], do: :preparations

  defp subject_for_type(type) when type in [:create, :update, :destroy], do: Ash.Changeset
  defp subject_for_type(:read), do: Ash.Query
  defp subject_for_type(:action), do: Ash.ActionInput
end
