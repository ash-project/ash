defmodule Ash.Dsl.Transformer do
  @moduledoc """
  A transformer manipulates and/or validates the entire DSL state of a resource.

  It's `transform/2` takes a `map`, which is just the values/configurations at each point
  of the DSL. Don't manipulate it directly, if possible, instead use functions like
  `get_entities/3` and `replace_entity/5` to manipulate it.

  Use the `after?/1` and `before?/1` callbacks to ensure that your transformer
  runs either before or after some other transformer.

  The pattern for requesting information from other modules that use the DSL and are
  also currently compiling has not yet been determined. If you have that requirement
  you will need extra utilities to ensure that some other DSL based module has either
  completed or reached a certain point in its transformers. These utilities have not
  yet been written.
  """
  @callback transform(Ash.resource(), map) :: {:ok, map} | {:error, term}
  @callback before?(module) :: boolean
  @callback after?(module) :: boolean

  defmacro __using__(_) do
    quote do
      @behaviour Ash.Dsl.Transformer

      def before?(_), do: false
      def after?(_), do: false

      defoverridable before?: 1, after?: 1
    end
  end

  def build_entity(extension, path, name, opts) do
    do_build_entity(extension.sections(), path, name, opts)
  end

  defp do_build_entity(sections, [section_name], name, opts) do
    section = Enum.find(sections, &(&1.name == section_name))
    entity = Enum.find(section.entities, &(&1.name == name))

    case NimbleOptions.validate(opts, entity.schema) do
      {:ok, opts} -> {:ok, struct(entity.target, opts)}
      {:error, error} -> {:error, error}
    end
  end

  defp do_build_entity(sections, [section_name | rest], name, opts) do
    section = Enum.find(sections, &(&1.name == section_name))

    do_build_entity(section.sections, rest, name, opts)
  end

  def add_entity(dsl_state, path, extension, entity) do
    Map.update(dsl_state, {path, extension}, %{entities: [entity], opts: []}, fn config ->
      Map.update(config, :entities, [entity], fn entities ->
        [entity | entities]
      end)
    end)
  end

  def get_entities(dsl_state, path, extension) do
    dsl_state
    |> Map.get({path, extension}, %{entities: []})
    |> Map.get(:entities, [])
  end

  @doc """
  Store a value in a special persistent term key, that will be copied to the runtime
  """
  def persist_to_runtime(module, key, value) do
    :persistent_term.put(key, value)
    current_persisted = :persistent_term.get({module, :persist_to_runtime}, [])

    unless key in current_persisted do
      :persistent_term.put({module, :persist_to_runtime}, [key | current_persisted])
    end

    :ok
  end

  def replace_entity(dsl_state, path, extension, replacement, matcher) do
    Map.update(dsl_state, {path, extension}, %{entities: [replacement], opts: []}, fn config ->
      Map.update(config, :entities, [replacement], fn entities ->
        replace_match(entities, replacement, matcher)
      end)
    end)
  end

  defp replace_match(entities, replacement, matcher) do
    Enum.map(entities, fn entity ->
      if matcher.(entity) do
        replacement
      else
        entity
      end
    end)
  end

  def sort(transformers) do
    transformers
    |> Enum.map(&build_before_after_list(&1, transformers))
    |> Enum.sort_by(& &1, __MODULE__)
    |> Enum.map(&elem(&1, 0))
  end

  def build_before_after_list(transformer, transformers, already_touched \\ []) do
    transformers
    |> Enum.reject(&(&1 in already_touched))
    |> Enum.reject(&(&1 == transformer))
    |> Enum.reduce({transformer, [], []}, fn other_transformer,
                                             {transformer, before_list, after_list} ->
      {_, other_befores, other_afters} =
        build_before_after_list(other_transformer, transformers, [transformer | already_touched])

      cond do
        transformer.before?(other_transformer) ->
          {transformer, [other_transformer | before_list ++ other_befores], after_list}

        transformer.after?(other_transformer) ->
          {transformer, before_list, [other_transformer | after_list ++ other_afters]}

        true ->
          {transformer, before_list, after_list}
      end
    end)
  end

  def compare({left, after_left, before_left}, {right, after_right, before_right}) do
    cond do
      right in after_left ->
        :lt

      right in before_left ->
        :gt

      left in after_right ->
        :gt

      left in before_right ->
        :lt

      true ->
        cond do
          left == right ->
            :eq

          inspect(left) < inspect(right) ->
            :lt

          true ->
            :gt
        end
    end
  end
end
