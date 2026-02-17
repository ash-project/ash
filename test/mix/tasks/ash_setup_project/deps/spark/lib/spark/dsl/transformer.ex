# SPDX-FileCopyrightText: 2022 spark contributors <https://github.com/ash-project/spark/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Spark.Dsl.Transformer do
  @moduledoc """
  A transformer manipulates and/or validates the entire DSL state of a resource.

  It's `transform/1` takes a `map`, which is just the values/configurations at each point
  of the DSL. Don't manipulate it directly, if possible, instead use functions like
  `get_entities/3` and `replace_entity/4` to manipulate it.

  Use the `after?/1` and `before?/1` callbacks to ensure that your transformer
  runs either before or after some other transformer.

  Return `true` in `after_compile/0` to have the transformer run in an `after_compile` hook,
  but keep in mind that no modifications to the dsl structure will be retained, so there is no
  real point in modifying the dsl that you return.
  """

  @type warning() :: String.t() | {String.t(), :erl_anno.anno()}

  @callback transform(map) ::
              :ok
              | {:ok, map}
              | {:error, term}
              | {:warn, map, warning() | list(warning())}
              | :halt
  @callback before?(module) :: boolean
  @callback after?(module) :: boolean
  @callback after_compile?() :: boolean

  defmacro __using__(_) do
    quote generated: true do
      @behaviour Spark.Dsl.Transformer

      def before?(_), do: false
      def after?(_), do: false
      def after_compile?, do: false

      defoverridable before?: 1, after?: 1, after_compile?: 0
    end
  end

  @doc """
  Saves a value into the dsl config with the given key.

  This can be used to precompute some information and cache it onto the resource,
  or simply store a computed value. It can later be retrieved with `Spark.Dsl.Extension.get_persisted/3`.
  """
  def persist(dsl, key, value) do
    Map.update(dsl, :persist, %{key => value}, &Map.put(&1, key, value))
  end

  @doc """
  Runs the function in an async compiler.

  Use this for compiling new modules and having them compiled
  efficiently asynchronously.
  """
  def async_compile(dsl, fun) do
    task = Spark.Dsl.Extension.do_async_compile(fun)

    tasks = get_persisted(dsl, :spark_compile_tasks, [])
    persist(dsl, :spark_compile_tasks, [task | tasks])
  end

  @doc """
  Add a quoted expression to be evaluated in the DSL module's context.

  Use this *extremely sparingly*. It should almost never be necessary, unless building certain
  extensions that *require* the module in question to define a given function.

  What you likely want is either one of the DSL introspection functions, like `Spark.Dsl.Extension.get_entities/2`
  or `Spark.Dsl.Extension.get_opt/5)`. If you simply want to store a custom value that can be retrieved easily, or
  cache some precomputed information onto the resource, use `persist/3`.

  Provide the dsl state, bindings that should be unquote-able, and the quoted block
  to evaluate in the module. For example, if we wanted to support a `resource.primary_key()` function
  that would return the primary key (this is unnecessary, just an example), we might do this:

  ```elixir
  fields = the_primary_key_fields

  dsl_state =
    Spark.Dsl.Transformer.eval(
      dsl_state,
      [fields: fields],
      quote do
        def primary_key() do
          unquote(fields)
        end
      end
    )
  ```
  """
  def eval(dsl, bindings, block) do
    to_eval = {block, bindings}

    Map.update(
      dsl,
      :eval,
      [to_eval],
      &[to_eval | &1]
    )
  end

  def get_persisted(dsl, key, default \\ nil) do
    dsl
    |> Map.get(:persist, %{})
    |> Map.get(key, default)
  end

  def fetch_persisted(dsl, key) do
    dsl
    |> Map.get(:persist, %{})
    |> Map.fetch(key)
  end

  def build_entity!(extension, path, name, opts) do
    case build_entity(extension, path, name, opts) do
      {:ok, entity} ->
        entity

      {:error, error} ->
        if is_exception(error) do
          raise error
        else
          raise "Error building entity #{inspect(error)}"
        end
    end
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

    entity =
      if section do
        Enum.find(section.entities, &(&1.name == maybe_entity_name))
      end

    sub_entity =
      if entity do
        entity.entities
        |> Keyword.values()
        |> List.flatten()
        |> Enum.find(&(&1.name == maybe_nested_entity_name))
      end

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

    {before_validate_auto, after_validate_auto} =
      Keyword.split(entity.auto_set_fields || [], Keyword.keys(entity.schema))

    with {:ok, opts} <-
           Spark.Options.validate(
             Keyword.merge(opts, before_validate_auto),
             entity.schema
           ),
         opts <- Keyword.merge(opts, after_validate_auto) do
      result = struct(struct(entity.target, opts), entities)

      case Spark.Dsl.Entity.transform(entity.transform, result) do
        {:ok, built} ->
          Spark.Dsl.Entity.maybe_apply_identifier(built, entity.identifier)

        other ->
          other
      end
    else
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

  def remove_entity(dsl_state, path, func) do
    Map.update(dsl_state, path, %{entities: [], opts: []}, fn config ->
      Map.update(config, :entities, [], fn entities ->
        Enum.reject(entities, func)
      end)
    end)
  end

  def get_entities(dsl_state, path) do
    dsl_state
    |> Map.get(path, %{entities: []})
    |> Map.get(:entities, [])
  end

  def fetch_option(dsl_state, path, option) do
    dsl_state
    |> Map.get(path, Spark.Dsl.Extension.default_section_config())
    |> Map.get(:opts)
    |> Kernel.||([])
    |> Keyword.fetch(option)
  end

  @spec get_section_anno(map, list(atom)) :: :erl_anno.anno() | nil
  def get_section_anno(dsl_state, path) do
    dsl_state
    |> Map.get(path, %{})
    |> Map.get(:section_anno)
  end

  @spec get_opt_anno(map, list(atom), atom) :: :erl_anno.anno() | nil
  def get_opt_anno(dsl_state, path, option) do
    dsl_state
    |> Map.get(path, Spark.Dsl.Extension.default_section_config())
    |> Map.get(:opts_anno, [])
    |> Keyword.get(option)
  end

  def get_option(dsl_state, path, option, default \\ nil) do
    dsl_state
    |> Map.get(path, Spark.Dsl.Extension.default_section_config())
    |> Map.get(:opts)
    |> Kernel.||([])
    |> Keyword.get(option, default)
  end

  def set_option(dsl_state, path, option, value) do
    dsl_state
    |> Map.put_new(path, Spark.Dsl.Extension.default_section_config())
    |> Map.update!(path, fn existing_opts ->
      existing_opts
      |> Map.put_new(:opts, [])
      |> Map.put_new(:opts_anno, [])
      |> Map.update!(:opts, fn opts ->
        Keyword.put(opts, option, value)
      end)
    end)
  end

  def replace_entity(dsl_state, path, replacement, matcher \\ nil) do
    matcher =
      matcher ||
        fn record ->
          record.__struct__ == replacement.__struct__ and
            record.__identifier__ == replacement.__identifier__
        end

    Map.replace_lazy(dsl_state, path, fn config ->
      Map.replace_lazy(config, :entities, fn entities ->
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
    digraph = :digraph.new()

    transformers
    |> Enum.each(fn transformer ->
      :digraph.add_vertex(digraph, transformer)
    end)

    transformers
    |> Enum.each(fn left ->
      transformers
      |> Enum.each(fn right ->
        if left != right do
          left_before_right? = left.before?(right) || right.after?(left)
          left_after_right? = left.after?(right) || right.before?(left)

          cond do
            # This is annoying, but some modules have `def after?(_), do: true`
            # The idea being that they'd like to go after everything that isn't
            # explicitly after it. Same with `def before?(_), do: true`
            left_before_right? && left_after_right? ->
              :ok

            left_before_right? ->
              if right not in :digraph.out_neighbours(digraph, left) do
                :digraph.add_edge(digraph, left, right)
              end

            left_after_right? ->
              if left not in :digraph.out_neighbours(digraph, right) do
                :digraph.add_edge(digraph, right, left)
              end

            true ->
              :ok
          end
        end
      end)
    end)

    transformers = walk_rest(digraph, transformers)
    :digraph.delete(digraph)

    transformers
  end

  defp walk_rest(digraph, transformers, acc \\ []) do
    case :digraph.vertices(digraph) do
      [] ->
        Enum.reverse(acc)

      vertices ->
        case Enum.find(vertices, &(:digraph.in_neighbours(digraph, &1) == [])) do
          nil ->
            case Enum.find(vertices, &(:digraph.out_neighbours(digraph, &1) == [])) do
              nil ->
                vertex =
                  Enum.min_by(vertices, fn l ->
                    Enum.find_index(transformers, &(&1 == l))
                  end)

                :digraph.del_vertex(digraph, vertex)
                walk_rest(digraph, transformers, acc ++ [vertex])

              vertex ->
                :digraph.del_vertex(digraph, vertex)
                walk_rest(digraph, transformers, acc ++ [vertex])
            end

          vertex ->
            :digraph.del_vertex(digraph, vertex)
            walk_rest(digraph, transformers, [vertex | acc])
        end
    end
  end
end
