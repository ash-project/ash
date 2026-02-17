# SPDX-FileCopyrightText: 2022 spark contributors <https://github.com/ash-project/spark/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Spark.InfoGenerator do
  @moduledoc """
  Used to dynamically generate configuration functions for Spark extensions
  based on their DSL.

  ## Usage

  ```elixir
  defmodule MyConfig do
    use Spark.InfoGenerator, extension: MyDslExtension, sections: [:my_section]
  end
  ```
  """

  @type options :: [{:extension, module} | {:sections, [atom]}]

  @doc false
  @spec __using__(options) :: Macro.t()
  defmacro __using__(opts) do
    extension = Keyword.fetch!(opts, :extension) |> Macro.expand(__CALLER__)
    sections = Keyword.get(opts, :sections, [])

    quote do
      require Spark.InfoGenerator
      require unquote(extension)

      Spark.InfoGenerator.generate_config_functions(
        unquote(extension),
        unquote(sections)
      )

      Spark.InfoGenerator.generate_options_functions(
        unquote(extension),
        unquote(sections)
      )

      Spark.InfoGenerator.generate_entity_functions(
        unquote(extension),
        unquote(sections)
      )
    end
  end

  @doc """
  Given an extension and a list of DSL sections, generate an options function
  which returns a map of all configured options for a resource (including
  defaults).
  """
  @spec generate_options_functions(module, [atom]) :: Macro.t()
  defmacro generate_options_functions(extension, sections) do
    for {path, options} <- extension_sections_to_option_list(extension, sections) do
      function_name = :"#{Enum.join(path, "_")}_options"

      quote location: :keep do
        @doc """
        #{unquote(Enum.join(path, "."))} DSL options

        Returns a map containing the and any configured or default values.
        """
        @spec unquote(function_name)(dsl_or_extended :: module | map) :: %{required(atom) => any}
        def unquote(function_name)(dsl_or_extended) do
          import Spark.Dsl.Extension, only: [get_opt: 4]

          unquote(Macro.escape(options))
          |> Stream.map(fn option ->
            value =
              dsl_or_extended
              |> get_opt(option.path, option.name, Map.get(option, :default))

            {option.name, value}
          end)
          |> Stream.reject(&is_nil(elem(&1, 1)))
          |> Map.new()
        end

        defoverridable [{unquote(function_name), 1}]
      end
    end
  end

  @doc """
  Given an extension and a list of DSL sections, generate an entities function
  which returns a list of entities.
  """
  @spec generate_entity_functions(module, [atom]) :: Macro.t()
  defmacro generate_entity_functions(extension, sections) do
    entity_paths =
      extension.sections()
      |> Stream.filter(&(&1.name in sections))
      |> Stream.flat_map(&explode_section([], &1))
      |> Stream.filter(fn {_, section} -> section.patchable? || Enum.any?(section.entities) end)
      |> Stream.map(&elem(&1, 0))

    for path <- entity_paths do
      function_name = path |> Enum.join("_") |> String.to_atom()

      quote location: :keep do
        @doc """
        #{unquote(Enum.join(path, "."))} DSL entities
        """
        @spec unquote(function_name)(dsl_or_extended :: module | map) :: [struct]
        def unquote(function_name)(dsl_or_extended) do
          import Spark.Dsl.Extension, only: [get_entities: 2]

          get_entities(dsl_or_extended, unquote(path))
        end

        defoverridable [{unquote(function_name), 1}]
      end
    end
  end

  @doc """
  Given an extension and a list of DSL sections generate individual config
  functions for each option.
  """
  @spec generate_config_functions(module, [atom]) :: Macro.t()
  defmacro generate_config_functions(extension, sections) do
    for {_, options} <- extension_sections_to_option_list(extension, sections) do
      for option <- options do
        generate_config_function(option)
      end
    end
  end

  defp explode_section(path, %{sections: [], name: name} = section),
    do: [{path ++ [name], section}]

  defp explode_section(path, %{sections: sections, name: name} = section) do
    path = path ++ [name]

    head = [{path, section}]
    tail = Stream.flat_map(sections, &explode_section(path, &1))

    Stream.concat(head, tail)
  end

  # sobelow_skip ["DOS.StringToAtom"]
  defp extension_sections_to_option_list(extension, sections) do
    extension.sections()
    |> Stream.filter(&(&1.name in sections))
    |> Stream.flat_map(&explode_section([], &1))
    |> Stream.reject(fn {_, section} -> Enum.empty?(section.schema) end)
    |> Stream.map(fn {path, section} ->
      schema =
        section.schema
        |> Enum.map(fn {name, opts} ->
          opts
          |> Map.new()
          |> Map.take(~w[type doc default]a)
          |> Map.update!(:type, &spec_for_type(&1, opts))
          |> Map.put(:pred?, name |> to_string() |> String.ends_with?("?"))
          |> Map.put(:name, name)
          |> Map.put(:path, path)
          |> Map.put(
            :function_name,
            path
            |> Enum.concat([name])
            |> Enum.join("_")
            |> String.trim_trailing("?")
            |> String.to_atom()
          )
        end)

      {path, schema}
    end)
    |> Map.new()
  end

  # sobelow_skip ["DOS.BinToAtom"]
  defp generate_config_function(%{pred?: true} = option) do
    function_name = :"#{option.function_name}?"

    quote location: :keep do
      @doc unquote(option.doc)
      @spec unquote(function_name)(dsl_or_extended :: module | map) ::
              unquote(option.type)
      def unquote(function_name)(dsl_or_extended) do
        import Spark.Dsl.Extension, only: [get_opt: 4]

        get_opt(
          dsl_or_extended,
          unquote(option.path),
          unquote(option.name),
          unquote(Macro.escape(option.default))
        )
      end

      defoverridable [{unquote(function_name), 1}]
    end
  end

  # sobelow_skip ["DOS.BinToAtom"]
  defp generate_config_function(option) do
    option = Map.put_new(option, :default, nil)

    quote location: :keep do
      @doc unquote(Map.get(option, :doc, false))
      @spec unquote(option.function_name)(dsl_or_extended :: module | map) ::
              {:ok, unquote(option.type)} | :error

      if unquote(is_nil(option.default)) do
        def unquote(option.function_name)(dsl_or_extended) do
          import Spark.Dsl.Extension, only: [fetch_opt: 3]

          case fetch_opt(
                 dsl_or_extended,
                 unquote(option.path),
                 unquote(option.name)
               ) do
            :error -> :error
            {:ok, value} -> {:ok, value}
          end
        end
      else
        def unquote(option.function_name)(dsl_or_extended) do
          import Spark.Dsl.Extension, only: [fetch_opt: 3]

          case fetch_opt(
                 dsl_or_extended,
                 unquote(option.path),
                 unquote(option.name)
               ) do
            :error -> {:ok, unquote(Macro.escape(option.default))}
            {:ok, value} -> {:ok, value}
          end
        end
      end

      @doc unquote(Map.get(option, :doc, false))
      @spec unquote(:"#{option.function_name}!")(dsl_or_extended :: module | map) ::
              unquote(option.type) | no_return
      if unquote(is_nil(option.default)) do
        # sobelow_skip ["DOS.BinToAtom"]
        def unquote(:"#{option.function_name}!")(dsl_or_extended) do
          import Spark.Dsl.Extension, only: [fetch_opt: 3, get_persisted: 2]

          case fetch_opt(
                 dsl_or_extended,
                 unquote(option.path),
                 unquote(option.name)
               ) do
            :error ->
              on = get_persisted(dsl_or_extended, :module)
              raise "No configuration for `#{unquote(option.name)}` present on #{inspect(on)}"

            {:ok, value} ->
              value
          end
        end
      else
        # sobelow_skip ["DOS.BinToAtom"]
        def unquote(:"#{option.function_name}!")(dsl_or_extended) do
          import Spark.Dsl.Extension, only: [fetch_opt: 3, get_persisted: 2]

          case fetch_opt(
                 dsl_or_extended,
                 unquote(option.path),
                 unquote(option.name)
               ) do
            :error ->
              unquote(Macro.escape(option.default))

            {:ok, value} ->
              value
          end
        end
      end

      defoverridable [
        {unquote(option.function_name), 1},
        {unquote(:"#{option.function_name}!"), 1}
      ]
    end
  end

  def spec_for_type(:keyword_list, opts) do
    if opts[:keys] do
      value_type =
        opts[:keys]
        |> Enum.map(fn {key, spec} ->
          key_spec =
            case key do
              :* -> spec_for_type(:atom, [])
              other -> other
            end

          {key_spec, spec_for_type(spec[:type], spec)}
        end)
        |> Enum.uniq()
        |> Enum.reduce(nil, fn
          type, nil ->
            type

          type, other_type ->
            {:|, [], [type, other_type]}
        end)

      [value_type]
    else
      quote do
        keyword()
      end
    end
  end

  # TODO: this should be combined with `Spark.Options.Docs.type_to_spec
  def spec_for_type({:literal, value}, _opts) when is_atom(value) or is_integer(value) do
    value
  end

  def spec_for_type({:literal, value}, _opts) when is_tuple(value) do
    value
    |> Tuple.to_list()
    |> Enum.map(&spec_for_type({:literal, &1}, []))
    |> List.to_tuple()
  end

  def spec_for_type({:literal, value}, _opts) when is_list(value) do
    Enum.map(value, &spec_for_type({:literal, &1}, []))
  end

  def spec_for_type({:literal, _value}, _opts) do
    {:any, [], Elixir}
  end

  def spec_for_type({:behaviour, _module}, _opts), do: {:module, [], Elixir}

  def spec_for_type({:spark, _}, _opts), do: {:module, [], Elixir}

  def spec_for_type({:spark_function_behaviour, behaviour, {_, arity}}, _opts),
    do:
      spec_for_type(
        {:or,
         [
           {:behaviour, behaviour},
           {{:behaviour, behaviour}, {:keyword, [], Elixir}},
           {:fun, arity}
         ]},
        []
      )

  def spec_for_type({:fun, arity}, _opts) do
    args = List.duplicate({:any, [], Elixir}, arity)

    [{:->, [], [args, {:any, [], Elixir}]}]
  end

  # Treat `and` like `or` because any of the input types is valid.
  def spec_for_type({:and, subtypes}, opts), do: spec_for_type({:or, subtypes}, opts)

  def spec_for_type({:or, [type]}, _opts), do: spec_for_type(type, [])

  def spec_for_type({:or, [next | remaining]}, _opts),
    do: {:|, [], [spec_for_type(next, []), spec_for_type({:or, remaining}, [])]}

  def spec_for_type({:in, %Range{first: first, last: last}}, _opts)
      when is_integer(first) and is_integer(last),
      do: {:.., [], [first, last]}

  def spec_for_type({:in, %Range{first: first, last: last}}, _opts),
    do:
      {{:., [], [{:__aliases__, [], [:Range]}, :t]}, [],
       [spec_for_type(first, []), spec_for_type(last, [])]}

  def spec_for_type({:in, values}, _opts) do
    values
    |> Enum.map(&spec_for_type({:literal, &1}, []))
    |> case do
      [] ->
        {:any, [], Elixir}

      specs ->
        Enum.reduce(specs, nil, fn
          spec, nil ->
            spec

          spec, other_specs ->
            {:|, [], [spec, other_specs]}
        end)
    end
  end

  def spec_for_type({type, subtype}, _opts) when type in [:list, :wrap_list],
    do: [spec_for_type(subtype, [])]

  def spec_for_type({:custom, _, _, _}, _opts), do: spec_for_type(:any, [])

  def spec_for_type({:tuple, subtypes}, _opts) do
    subtypes
    |> Enum.map(&spec_for_type(&1, []))
    |> List.to_tuple()
  end

  def spec_for_type(:quoted, _opts) do
    {:any, [], Elixir}
  end

  def spec_for_type(:string, _opts),
    do: {{:., [], [{:__aliases__, [alias: false], [:String]}, :t]}, [], []}

  def spec_for_type(:literal, _opts) do
    {:any, [], Elixir}
  end

  def spec_for_type(terminal, _opts)
      when terminal in ~w[any map atom string boolean integer non_neg_integer pos_integer float timeout pid reference mfa]a,
      do: {terminal, [], Elixir}

  def spec_for_type(atom, _opts) when is_atom(atom), do: atom
  def spec_for_type(number, _opts) when is_number(number), do: number
  def spec_for_type(string, _opts) when is_binary(string), do: spec_for_type(:string, [])

  def spec_for_type({:one_of, values}, opts), do: spec_for_type({:in, values}, opts)

  def spec_for_type({mod, arg}, _opts) when is_atom(mod) and is_list(arg),
    do: {{:module, [], Elixir}, {:list, [], Elixir}}

  def spec_for_type({:map, key_type, value_type}, _opts) do
    {:%{}, [], [{{:optional, [], [spec_for_type(key_type, [])]}, spec_for_type(value_type, [])}]}
  end

  def spec_for_type(:map, opts) do
    spec_for_type({:map, :atom, :any}, opts)
  end

  def spec_for_type(tuple, _opts) when is_tuple(tuple) do
    {:tuple, [], []}
  end

  def spec_for_type([], _), do: []
  def spec_for_type([type], opts), do: [spec_for_type(type, opts)]
end
