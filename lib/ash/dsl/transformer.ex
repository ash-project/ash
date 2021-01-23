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
  @callback transform(module, map) :: {:ok, map} | {:error, term} | :halt
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

  def persist(dsl, key, value) do
    Map.update(dsl, :persist, %{key => value}, &Map.put(&1, key, value))
  end

  def get_persisted(dsl, key, default \\ nil) do
    dsl
    |> Map.get(:persist, %{})
    |> Map.get(key, default)
  end

  def build_entity(extension, path, name, opts) do
    do_build_entity(extension.sections(), path, name, opts)
  end

  defp do_build_entity(sections, [section_name], name, opts) do
    section = Enum.find(sections, &(&1.name == section_name))
    entity = Enum.find(section.entities, &(&1.name == name))

    case Ash.OptionsHelpers.validate(opts, entity.schema) do
      {:ok, opts} ->
        {:ok, struct(entity.target, opts)}

      {:error, error} ->
        {:error, error}
    end
  end

  defp do_build_entity(sections, [section_name | rest], name, opts) do
    section = Enum.find(sections, &(&1.name == section_name))

    do_build_entity(section.sections, rest, name, opts)
  end

  def add_entity(dsl_state, path, entity) do
    Map.update(dsl_state, path, %{entities: [entity], opts: []}, fn config ->
      Map.update(config, :entities, [entity], fn entities ->
        [entity | entities]
      end)
    end)
  end

  def get_entities(dsl_state, path) do
    dsl_state
    |> Map.get(path, %{entities: []})
    |> Map.get(:entities, [])
  end

  def get_option(dsl_state, path, option) do
    dsl_state
    |> Map.get(path, %{opts: []})
    |> Map.get(:opts)
    |> Kernel.||([])
    |> Keyword.get(option)
  end

  def replace_entity(dsl_state, path, replacement, matcher) do
    Map.update(dsl_state, path, %{entities: [replacement], opts: []}, fn config ->
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
    Enum.reduce(transformers, [], fn transformer, list ->
      put_transformer_in(list, transformer)
    end)
  end

  defp put_transformer_in([], transformer), do: [transformer]

  defp put_transformer_in([first | rest] = remaining, transformer) do
    if transformer.before?(first) or first.after?(transformer) do
      [transformer | remaining]
    else
      [first | put_transformer_in(rest, transformer)]
    end
  end
end
