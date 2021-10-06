defmodule Ash.Dsl.Transformer do
  @moduledoc """
  A transformer manipulates and/or validates the entire DSL state of a resource.

  It's `transform/2` takes a `map`, which is just the values/configurations at each point
  of the DSL. Don't manipulate it directly, if possible, instead use functions like
  `get_entities/3` and `replace_entity/5` to manipulate it.

  Use the `after?/1` and `before?/1` callbacks to ensure that your transformer
  runs either before or after some other transformer.

  Return `true` in `after_compile/0` to have the transformer run in an `after_compile` hook,
  but keep in mind that no modifications to the dsl structure will be retained, so there is no
  point in returning a new dsl structure from `transform/2` if `after_compile/0` is defined. Instead,
  simply return `:ok` or `{:error, error}`
  """
  @callback transform(module, map) :: :ok | {:ok, map} | {:error, term} | :halt
  @callback before?(module) :: boolean
  @callback after?(module) :: boolean
  @callback after_compile?() :: boolean

  defmacro __using__(_) do
    quote do
      @behaviour Ash.Dsl.Transformer

      def before?(_), do: false
      def after?(_), do: false
      def after_compile?, do: false

      defoverridable before?: 1, after?: 1, after_compile?: 0
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

    do_build(entity, opts)
  end

  defp do_build_entity(
         sections,
         [section_name, maybe_entity_name],
         maybe_nested_entity_name,
         opts
       ) do
    section = Enum.find(sections, &(&1.name == section_name))
    entity = Enum.find(section.entities, &(&1.name == maybe_entity_name))

    sub_entity =
      entity.entities
      |> Keyword.values()
      |> List.flatten()
      |> Enum.find(&(&1.name == maybe_nested_entity_name))

    if sub_entity do
      do_build(sub_entity, opts)
    else
      do_build_entity(section.sections, [maybe_entity_name], maybe_nested_entity_name, opts)
    end
  end

  defp do_build_entity(sections, [section_name | rest], name, opts) do
    section = Enum.find(sections, &(&1.name == section_name))
    do_build_entity(section.sections, rest, name, opts)
  end

  defp do_build(entity, opts) do
    entity_names =
      entity.entities
      |> Kernel.||([])
      |> Keyword.keys()

    {entities, opts} = Keyword.split(opts, entity_names)

    case Ash.OptionsHelpers.validate(opts, entity.schema) do
      {:ok, opts} ->
        result = struct(struct(entity.target, opts), entities)
        Ash.Dsl.Entity.transform(entity.transform, result)

      {:error, error} ->
        {:error, error}
    end
  end

  def add_entity(dsl_state, path, entity, opts \\ []) do
    Map.update(dsl_state, path, %{entities: [entity], opts: []}, fn config ->
      Map.update(config, :entities, [entity], fn entities ->
        if (opts[:type] || :prepend) == :prepend do
          [entity | entities]
        else
          entities ++ [entity]
        end
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

  def set_option(dsl_state, path, option, value) do
    dsl_state
    |> Map.put_new(path, %{opts: []})
    |> Map.update!(path, fn existing_opts ->
      existing_opts
      |> Map.put_new(:opts, [])
      |> Map.update!(:opts, fn opts ->
        Keyword.put(opts, option, value)
      end)
    end)
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
