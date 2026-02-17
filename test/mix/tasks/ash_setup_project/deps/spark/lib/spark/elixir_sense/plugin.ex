# SPDX-FileCopyrightText: 2022 spark contributors <https://github.com/ash-project/spark/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Spark.ElixirSense.Plugin do
  @moduledoc false

  Module.register_attribute(__MODULE__, :is_elixir_sense_plugin, persist: true)
  @is_elixir_sense_plugin true

  @generic_reducer nil

  @matcher ElixirSense.Providers.Suggestion.Matcher

  case Code.ensure_compiled(ElixirSense.Plugin) do
    {:module, _} ->
      @behaviour ElixirSense.Plugin
      @behaviour ElixirSense.Providers.Suggestion.GenericReducer
      @generic_reducer ElixirSense.Providers.Suggestion.GenericReducer

    _ ->
      :ok
  end

  case Code.ensure_compiled(ElixirSense.Providers.Plugin) do
    {:module, _} ->
      @behaviour ElixirSense.Providers.Plugin
      @behaviour ElixirSense.Providers.Completion.GenericReducer
      @generic_reducer ElixirSense.Providers.Completion.GenericReducer
      @matcher ElixirSense.Providers.Utils.Matcher

    _ ->
      :ok
  end

  case Code.ensure_compiled(ElixirLS.LanguageServer.Plugin) do
    {:module, _} ->
      @behaviour ElixirLS.LanguageServer.Plugin
      @behaviour ElixirLS.LanguageServer.Providers.Completion.GenericReducer
      @generic_reducer ElixirLS.LanguageServer.Providers.Completion.GenericReducer
      @matcher ElixirLS.Utils.Matcher

    _ ->
      :ok
  end

  if @generic_reducer do
    def reduce(hint, env, buffer_metadata, cursor_context, acc) do
      apply(@generic_reducer, :reduce, [
        __MODULE__,
        hint,
        env,
        buffer_metadata,
        cursor_context,
        acc
      ])
    end
  end

  alias Spark.ElixirSense.Entity
  alias Spark.ElixirSense.Types

  def suggestions(hint, _, _chain, opts) do
    case suggestions(hint, opts) do
      :ignore ->
        case func_call_chain(opts.cursor_context.text_before, opts.env) do
          [%{candidate: {module, function_call}, npar: arg_index} = info | _] ->
            autocomplete_spark_options(hint, opts, {module, function_call, arg_index, info}, opts)

          _ ->
            :ignore
        end

      suggestions ->
        suggestions
    end
  rescue
    _e ->
      :ignore
  end

  def suggestions(hint, opts) do
    is_spark_entity? = do_dsl?(opts.env) || fragment?(opts.env)

    if is_spark_entity? do
      case func_call_chain(opts.cursor_context.text_before, opts.env) do
        [%{candidate: {Elixir, :use}, params: [mod | _]} = info | _] ->
          {using, _} = get_mod(mod, opts.env)

          if Code.ensure_loaded?(using) && Spark.implements_behaviour?(using, Spark.Dsl) do
            opt_schema = using.opt_schema()

            if opt_schema do
              case autocomplete_schema(opt_schema, hint, info.value_type_path, opts) do
                [] ->
                  :ignore

                completions ->
                  {:override, Enum.uniq(completions)}
              end
            else
              :ignore
            end
          else
            :ignore
          end

        [%{candidate: {module, function_call}, npar: arg_index} = info | _] = chain ->
          chain
          |> Enum.split_while(fn
            %{candidate: {Elixir, :defmodule}} -> false
            _ -> true
          end)
          |> case do
            {_, []} ->
              autocomplete_spark_options(
                hint,
                opts,
                {module, function_call, arg_index, info},
                opts
              )

            {path_items, _} ->
              scope_path =
                path_items
                |> Enum.map(fn %{candidate: {_, function_call}} -> function_call end)
                |> Enum.reverse()

              get_suggestions(hint, opts, scope_path, info.value_type_path, arg_index)
          end

        _ ->
          :ignore
      end
    else
      case func_call_chain(opts.cursor_context.text_before, opts.env) do
        [%{candidate: {module, function_call}, npar: arg_index} = info | _] ->
          autocomplete_spark_options(hint, opts, {module, function_call, arg_index, info}, opts)

        _ ->
          :ignore
      end
    end
  rescue
    _e ->
      :ignore
  end

  defp autocomplete_schema(
         schema,
         hint,
         [{:keyword_key, key, _rest_keyword} | value_type_path],
         opts
       ) do
    case Keyword.fetch(schema, key) do
      :error ->
        []

      {:ok, config} ->
        option_values(key, config, hint, opts, value_type_path)
    end
  end

  defp autocomplete_schema(schema, hint, [{:making_keyword_key, other_opts}], _opts) do
    other_keys =
      other_opts
      |> Enum.reject(&cursor?/1)
      |> Enum.map(&elem(&1, 0))

    schema
    |> Keyword.drop(other_keys)
    |> Enum.filter(fn {key, _} ->
      apply(@matcher, :match?, [to_string(key), hint])
    end)
    |> Enum.map(fn {key, config} ->
      option_suggestions(key, config, :option)
    end)
  end

  defp autocomplete_schema(schema, hint, [], _opts) do
    schema
    |> Enum.filter(fn {key, _} ->
      apply(@matcher, :match?, [to_string(key), hint])
    end)
    |> Enum.map(fn {key, config} ->
      option_suggestions(key, config, :option)
    end)
  end

  defp autocomplete_schema(_schema, _hint, _value_type_path, _opts) do
    []
  end

  defp func_call_chain(code, env) do
    case Code.Fragment.container_cursor_to_quoted(code, columns: true) do
      {:ok, container} ->
        container
        |> cursor_path()
        |> Enum.reverse()
        |> rewrite_pipes()
        |> collapse_do_blocks()
        |> collapse_lists()
        |> Enum.reverse()
        |> to_partials()
        |> path_to_mod_fun_arg_info(env)
        |> Enum.reverse()
        |> handle_dangling_do_blocks()

      _ ->
        []
    end
  end

  defp rewrite_pipes([{:|>, _, [params_1, {call, meta, params_rest}]}, _ | rest]) do
    rewrite_pipes([{call, meta, [params_1 | params_rest || []]} | rest])
  end

  defp rewrite_pipes([first | rest]) do
    [first | rewrite_pipes(rest)]
  end

  defp rewrite_pipes([]), do: []

  defp handle_dangling_do_blocks([%{value_type_path: [{:keyword_key, :do, []}]} = info | rest]) do
    [%{info | value_type_path: [:do_block]} | rest]
  end

  defp handle_dangling_do_blocks(other), do: other

  defp collapse_do_blocks([first | rest]) do
    case first do
      {_func, _, args} ->
        case List.last(args) do
          [{:do, _block}] ->
            [first | collapse_do_blocks(Enum.drop(rest, 2))]

          _ ->
            [first | collapse_do_blocks(rest)]
        end

      _ ->
        [first, collapse_do_blocks(rest)]
    end
  end

  defp collapse_do_blocks([]), do: []

  defp collapse_lists([first, _second | rest]) when is_list(first) do
    [first | collapse_lists(rest)]
  end

  defp collapse_lists([first | rest]), do: [first | collapse_lists(rest)]

  defp collapse_lists([]), do: []

  defp path_to_mod_fun_arg_info(path, env) do
    path
    |> Enum.reduce_while({[], []}, fn
      {:__block__, _, _items}, {acc, stack} ->
        {:cont, {acc, [:block | stack]}}

      item, {acc, stack} ->
        case to_mod_fun_arg_info(item, env) do
          {:ok, item} ->
            item = %{item | value_type_path: merge_do_and_blocks(stack ++ item.value_type_path)}
            {:cont, {[item | acc], []}}

          _ ->
            case cursor_at_value_type_path(item) do
              nil ->
                {:cont, {acc, stack}}

              path ->
                {:cont, {acc, stack ++ path}}
            end
        end
    end)
    |> elem(0)
  end

  defp merge_do_and_blocks([]), do: []

  defp merge_do_and_blocks([:block, {:keyword_key, :do, _} | rest]) do
    merge_do_and_blocks([:do_block | rest])
  end

  defp merge_do_and_blocks([first | rest]) do
    [first | merge_do_and_blocks(rest)]
  end

  defp to_partials(items, last \\ :___none___)
  defp to_partials([item], :__none__), do: [item]

  defp to_partials([item], last) do
    [replace_with_cursor(item, last)]
  end

  defp to_partials([], _), do: []

  defp to_partials([first, second | rest], :__none__) do
    [replace_with_cursor(second, first) | to_partials(rest, second)]
  end

  defp to_partials([first | rest], last) do
    [replace_with_cursor(first, last) | to_partials(rest, first)]
  end

  defp replace_with_cursor(inside, replace) do
    Macro.prewalk(inside, fn
      ^replace ->
        {:__cursor__, [], []}

      other ->
        other
    end)
  end

  @excluded_funs [:__block__]

  defp to_mod_fun_arg_info(item, binding_env) do
    with {_, {:ok, call_info}} <-
           Macro.prewalk(item, nil, &find_call_pre/2),
         {{m, elixir_prefix}, f} when f not in @excluded_funs <-
           get_mod_fun(
             call_info.call,
             binding_env
           ) do
      {:ok,
       %{
         candidate: {m, f},
         elixir_prefix: elixir_prefix,
         params: call_info.params,
         npar: call_info.npar,
         pos: {{call_info.meta[:line], call_info.meta[:column]}, {call_info.meta[:line], nil}},
         value_type_path: call_info[:value_type_path] || [],
         cursor_at_option: call_info.cursor_at_option,
         options_so_far: call_info.options,
         option: call_info.option
       }}
    else
      _other ->
        :error
    end
  end

  defp cursor_path(container) do
    Macro.path(container, fn
      {:__cursor__, _, _} ->
        true

      _ ->
        false
    end)
  end

  defp find_call_pre(ast, {:ok, call_info}),
    do: {ast, {:ok, call_info}}

  # transform `a |> b(c)` calls into `b(a, c)`
  defp find_call_pre({:|>, _, [params_1, {call, meta, params_rest}]}, state) do
    params = [params_1 | params_rest || []]
    find_call_pre({call, meta, params}, state)
  end

  defp find_call_pre({{:., meta, call}, _, params} = ast, _state) when is_list(params) do
    {ast, find_cursor_in_params(params, call, meta)}
  end

  # Special handling for use calls
  defp find_call_pre({:use, meta, [module_ast | params]} = ast, _state) do
    {ast, find_cursor_in_params([module_ast | params], [module_ast, :use], meta)}
  end

  defp find_call_pre({atom, meta, params} = ast, _state)
       when is_atom(atom) and is_list(params) and atom not in [:{}, :%{}] do
    {ast, find_cursor_in_params(params, atom, meta)}
  end

  defp find_call_pre(ast, state), do: {ast, state}

  defp get_mod_fun(atom, _binding_env) when is_atom(atom), do: {{Elixir, atom}, atom}

  defp get_mod_fun([{:__aliases__, _, list}, fun], binding_env) do
    mod = get_mod(list, binding_env)

    if mod do
      {mod, fun}
    end
  end

  defp get_mod_fun([{:__MODULE__, _, nil}, fun], binding_env) do
    if binding_env.current_module not in [nil, Elixir] do
      {{binding_env.current_module, false}, fun}
    end
  end

  defp get_mod_fun([{:@, _, [{name, _, nil}]}, fun], binding_env) when is_atom(name) do
    case apply(ElixirSense.Core.Binding, :expand, [binding_env, {:attribute, name}]) do
      {:atom, atom} ->
        {{atom, false}, fun}

      _ ->
        nil
    end
  end

  defp get_mod_fun([{name, _, nil}, fun], binding_env) when is_atom(name) do
    case apply(ElixirSense.Core.Binding, :expand, [binding_env, {:variable, name}]) do
      {:atom, atom} ->
        {{atom, false}, fun}

      _ ->
        nil
    end
  end

  defp get_mod_fun([atom, fun], _binding_env) when is_atom(atom), do: {{atom, false}, fun}
  defp get_mod_fun(_, _binding_env), do: nil

  defp get_mod({:__aliases__, _, list}, binding_env) do
    get_mod(list, binding_env)
  end

  defp get_mod([{:__aliases__, _, list} | _rest], binding_env) do
    get_mod(list, binding_env)
  end

  defp get_mod([{:__MODULE__, _, nil} | rest], binding_env) do
    if binding_env.current_module not in [nil, Elixir] do
      mod =
        binding_env.current_module
        |> Module.split()
        |> Kernel.++(rest)
        |> Module.concat()

      {mod, false}
    end
  end

  defp get_mod([{:@, _, [{name, _, nil}]} | rest], binding_env) when is_atom(name) do
    case apply(ElixirSense.Core.Binding, :expand, [binding_env, {:attribute, name}]) do
      {:atom, atom} ->
        if apply(ElixirSense.Core.Introspection, :elixir_module?, [atom]) do
          mod =
            atom
            |> Module.split()
            |> Kernel.++(rest)
            |> Module.concat()

          {mod, false}
        else
          nil
        end

      _ ->
        nil
    end
  end

  defp get_mod([head | _rest] = list, _binding_env) when is_atom(head) do
    {Module.concat(list), head == Elixir}
  end

  defp get_mod(_list, _binding_env), do: nil

  defp find_cursor_in_params(params, call, meta) do
    params
    |> Enum.with_index()
    |> Enum.find_value(fn {param, index} ->
      case cursor_at_value_type_path(param) do
        nil ->
          nil

        path ->
          {:ok,
           %{
             call: call,
             params: params,
             npar: index,
             meta: meta,
             value_type_path: path,
             options: nil,
             cursor_at_option: false,
             option: nil
           }}
      end
    end)
  end

  defp cursor?({:__cursor__, _, _}), do: true
  defp cursor?(_), do: false

  defp cursor_at_value_type_path([{:do, _, block}]) do
    Enum.find_value(List.wrap(block), fn item ->
      case cursor_at_value_type_path(item) do
        nil ->
          nil

        path ->
          [:do_block | path]
      end
    end)
  end

  defp cursor_at_value_type_path([{:__cursor__, _, _}]) do
    [{:list_index, 0, []}]
  end

  defp cursor_at_value_type_path(other) when is_list(other) do
    if Keyword.keyword?(Enum.reject(other, &cursor?/1)) do
      Enum.find_value(other, fn
        {:__cursor__, _, _} = cursor ->
          [
            {:making_keyword_key, Enum.reject(other, &(&1 == cursor))}
          ]

        {key, value} ->
          case cursor_at_value_type_path(value) do
            nil ->
              nil

            path ->
              [{:keyword_key, key, Keyword.delete(other, key)} | path]
          end
      end)
    else
      other
      |> Enum.with_index()
      |> Enum.find_value(fn {value, index} ->
        case cursor_at_value_type_path(value) do
          nil ->
            nil

          path ->
            [{:list_index, index} | path]
        end
      end)
      |> case do
        [{:list_index, index} | path] ->
          [{:list_index, index, List.delete_at(path, index)} | path]

        nil ->
          nil
      end
    end
  end

  defp cursor_at_value_type_path({a, b}) do
    cursor_at_value_type_path({:{}, [], [a, b]})
  end

  defp cursor_at_value_type_path({:{}, _, elems}) do
    elems
    |> Enum.with_index()
    |> Enum.find_value(fn
      {{:__cursor__, _, _}, index} ->
        [{:making_tuple_elem, index, List.delete_at(elems, index)}]

      {value, index} ->
        case cursor_at_value_type_path(value) do
          nil ->
            nil

          path ->
            [{:tuple_index, index, List.delete_at(elems, index)} | path]
        end
    end)
  end

  defp cursor_at_value_type_path({:%{}, _, keys}) do
    Enum.find_value(keys, fn
      {:__cursor__, _, _} = cursor ->
        [{:making_map_key, Enum.reject(keys, &(&1 == cursor))}]

      {key, value} ->
        case cursor_at_value_type_path(value) do
          nil ->
            nil

          path ->
            [
              {:map_key, key, Enum.reject(keys, fn {other_key, _val} -> other_key == key end)}
              | path
            ]
        end
    end)
  end

  defp cursor_at_value_type_path({:__cursor__, _, []}), do: []

  defp cursor_at_value_type_path(_), do: nil

  defp autocomplete_spark_options(hint, _, {module, function_call, arg_index, info}, opts) do
    if (function_call == :use and arg_index > 0) && Code.ensure_loaded?(module) &&
         function_exported?(module, :opt_schema, 0) do
      schema = module.opt_schema()
      {:override, Enum.uniq(autocomplete_schema(schema, hint, info.value_type_path, opts))}
    else
      case Code.fetch_docs(module) do
        {:docs_v1, _a, :elixir, _b, _c, _d, functions} ->
          schema =
            Enum.find_value(functions, fn
              {{_, ^function_call, _}, _, _, _, %{spark_opts: spark_opts}} ->
                Enum.find_value(spark_opts, fn {index, schema} ->
                  if index == arg_index do
                    schema
                  end
                end)

              _ ->
                nil
            end)

          if schema do
            {:override, Enum.uniq(autocomplete_schema(schema, hint, info.value_type_path, opts))}
          else
            :ignore
          end

        _ ->
          :ignore
      end
    end
  end

  defp get_suggestions(hint, opts, scope_path, value_type_path, arg_index) do
    dsl_mod = get_dsl_mod(opts)

    {type, _value_type_path} =
      case value_type_path do
        [:do_block | rest] ->
          {:builder, rest}

        _ ->
          {:option, value_type_path}
      end

    if dsl_mod do
      extension_kinds =
        List.flatten(dsl_mod.module_info()[:attributes][:spark_extension_kinds] || [])

      extensions =
        default_extensions(dsl_mod)
        |> Enum.concat(parse_extensions(opts, extension_kinds))
        |> Enum.flat_map(fn extension ->
          [extension | extension.add_extensions()]
        end)

      case get_constructors(extensions, scope_path, hint, arg_index) do
        [] ->
          :ignore

        constructors ->
          suggestions =
            Enum.flat_map(constructors, fn
              {:value, key, config} ->
                List.wrap(option_values(key, config, hint, opts, value_type_path))

              {key, config} ->
                List.wrap(option_suggestions(key, config, type))

              %{__struct__: Spark.Dsl.Entity} = entity ->
                List.wrap(entity_suggestions(entity))

              %{__struct__: Spark.Dsl.Section} = section ->
                List.wrap(section_suggestions(section))
            end)
            |> filter_matches(hint)

          {:override, Enum.uniq(List.flatten(suggestions))}
      end
    else
      :ignore
    end
  end

  defp filter_matches(hints, match) do
    if match do
      Enum.filter(hints, fn
        %{label: label} ->
          apply(@matcher, :match?, [label, match])

        %{name: name} ->
          apply(@matcher, :match?, [name, match])
      end)
    else
      hints
    end
  end

  defp do_dsl?(env) do
    Enum.any?(env.attributes, &(&1.name == :spark_is)) ||
      Enum.any?(env.requires, fn module ->
        try do
          module.module_info(:attributes)
          |> Enum.any?(fn
            {:spark_dsl, [true]} ->
              true

            _ ->
              false
          end)
        rescue
          _ -> false
        end
      end)
  end

  defp fragment?(env) do
    Enum.any?(env.attributes, &(&1.name == :spark_fragment_of)) ||
      Spark.Dsl.Fragment in env.requires
  end

  defp get_dsl_mod(opts) do
    if Spark.Dsl.Fragment in opts.env.requires do
      parse_fragment(opts)
    else
      Enum.find(opts.env.requires, &spark_extension?/1)
    end
  end

  defp parse_fragment(opts) do
    case Regex.named_captures(
           ~r/of:\s+?(?<of>|([^\s]*))?[\,\s$]/,
           opts.cursor_context.text_before
         ) do
      %{"of" => dsl} when dsl != "" ->
        module = Module.concat([dsl])

        if Code.ensure_loaded?(module) do
          module
        end

      _ ->
        nil
    end
  rescue
    _ -> false
  end

  defp section_suggestions(section) do
    %{
      type: :generic,
      kind: :function,
      label: to_string(section.name),
      snippet: """
      #{section.name} do
        #{snippet_or_default(section.snippet, "$0")}
      end
      """,
      detail: "Dsl Section",
      documentation: Map.get(section, :docs) || ""
    }
  end

  defp entity_suggestions(entity) do
    %{
      type: :generic,
      kind: :function,
      label: to_string(entity.name),
      snippet: snippet_or_default(entity.snippet, "#{entity.name} #{args(entity)}"),
      detail: "Dsl Entity",
      documentation: Map.get(entity, :docs) || ""
    }
  end

  defp option_suggestions(key, config, type) do
    snippet = snippet_or_default(config[:snippet], default_snippet(config))

    snippet =
      if type == :option do
        "#{key}: #{snippet}"
      else
        "#{key} #{snippet}"
      end

    config = Spark.Options.update_key_docs(config)

    %{
      type: :generic,
      kind: :function,
      label: to_string(key),
      snippet: snippet,
      detail: "Option",
      documentation: config[:doc]
    }
  end

  defp option_values(key, config, hint, opts, value_type_path) do
    case config[:type] do
      :boolean ->
        enum_suggestion([true, false], hint, "boolean")

      {:one_of, values} ->
        enum_suggestion(values, hint, to_string(key))

      {:in, values} ->
        enum_suggestion(values, hint, to_string(key))

      {:spark_type, module, func} ->
        Types.find_builtin_types(module, func, hint, opts.cursor_context) ++
          Types.find_custom_types(module, func, hint, opts.module_store)

      {:spark_type, module, func, templates} ->
        Types.find_builtin_types(module, func, hint, opts.cursor_context, templates) ++
          Types.find_custom_types(module, func, hint, opts.module_store)

      {:spark, type} ->
        Entity.find_entities(type, hint)

      {:spark_behaviour, behaviour, builtins} ->
        Entity.find_spark_behaviour_impls(behaviour, builtins, hint, opts.module_store)

      {:spark_function_behaviour, behaviour, builtins, _} ->
        Entity.find_spark_behaviour_impls(behaviour, builtins, hint, opts.module_store)

      {:behaviour, behaviour} ->
        Entity.find_behaviour_impls(behaviour, hint, opts.module_store)

      {:spark_behaviour, behaviour} ->
        Entity.find_spark_behaviour_impls(behaviour, nil, hint, opts.module_store)

      {:spark_function_behaviour, behaviour, _} ->
        Entity.find_spark_behaviour_impls(behaviour, nil, hint, opts.module_store)

      {:or, subtypes} ->
        Enum.flat_map(subtypes, fn subtype ->
          option_values(key, Keyword.put(config, :type, subtype), hint, opts, value_type_path)
        end)

      {:list, subtype} ->
        case value_type_path do
          [{:list_index, _, _} | value_type_path] ->
            option_values(key, Keyword.put(config, :type, subtype), hint, opts, value_type_path)

          _ ->
            [
              %{
                type: :generic,
                kind: :class,
                snippet: inspect("[:$0]"),
                insert_text: "",
                detail: "list",
                documentation: "[]"
              }
            ]
        end

      {:wrap_list, subtype} ->
        case value_type_path do
          [{:list_index, _, _} | value_type_path] ->
            option_values(key, Keyword.put(config, :type, subtype), hint, opts, value_type_path)

          _ ->
            [
              %{
                type: :generic,
                kind: :class,
                snippet: inspect("[:$0]"),
                insert_text: "",
                detail: "list",
                documentation: "[]"
              }
              | option_values(
                  key,
                  Keyword.put(config, :type, subtype),
                  hint,
                  opts,
                  value_type_path
                )
            ]
        end

      _ ->
        []
    end
  end

  defp enum_suggestion(values, hint, label) do
    Enum.flat_map(values, fn v ->
      inspected = inspect(v)

      if apply(@matcher, :match?, [inspected, hint]) do
        [
          %{
            type: :generic,
            kind: :type_parameter,
            label: label,
            insert_text: inspect(v),
            snippet: inspect(v),
            documentation: inspect(v),
            priority: 0
          }
        ]
      else
        []
      end
    end)
  end

  defp default_snippet(config) do
    cond do
      config[:type] == :boolean && config[:default] in [true, false] ->
        "#{to_string(!config[:default])}"

      config[:type] == :string ->
        "\"$0\""

      match?({:list, _}, config[:type]) ->
        {_, inner_type} = config[:type]
        "[#{default_snippet(type: inner_type)}]"

      match?({:wrap_list, _}, config[:type]) ->
        {_, inner_type} = config[:type]
        default_snippet(type: inner_type)

      match?({in_type, _} when in_type in [:in, :one_of], config[:type]) ->
        {_, list} = config[:type]

        if is_nil(config[:default]) do
          {_, list} = config[:type]
          inspect(Enum.at(list, 0))
        else
          list |> Enum.reject(&(&1 == config[:default])) |> Enum.at(0) |> inspect()
        end

      match?({:tagged_tuple, _, _}, config[:type]) ->
        {:tagged_tuple, tag, inner_type} = config[:type]
        "{:#{tag}, #{default_snippet(inner_type)}}"

      config[:type] == :atom ->
        ":$0"

      match?({:mfa_or_fun, _}, config[:type]) ->
        {:mfa_or_fun, arity} = config[:type]
        default_snippet(type: {:or, [{:fun, arity}, :mfa]})

      match?({:literal, _}, config[:type]) ->
        {:literal, value} = config[:type]
        inspect(value)

      match?({:fun, 0}, config[:type]) ->
        """
        fn ->
          ${0:body}
        end
        """

      match?({:fun, _}, config[:type]) ->
        {:fun, arity} = config[:type]
        args = Enum.map_join(0..(arity - 1), ", ", &"arg#{&1 + 1}")

        """
        fn #{args} ->
          ${#{arity}:body}
        end
        """

      match?(:map, config[:type]) || match?({:map, _, _}, config[:type]) ->
        "%{${1:key} => ${2:value}}"

      match?(:keyword_list, config[:type]) ->
        "[${1:key} => ${2:value}]"

      config[:type] == :mfa ->
        "{${1:module}, :${2:function}, [${3:args}]}"

      config[:type] == :keyword_list ->
        "[$0]"

      true ->
        "$0"
    end
  end

  defp args(entity) do
    case Spark.Dsl.Entity.arg_names(entity) do
      [] ->
        ""

      args ->
        args
        |> Enum.with_index()
        |> Enum.map(fn {arg, index} ->
          "${#{index + 1}:#{arg}}"
        end)
        |> Enum.intersperse(", ")
        |> Enum.join()
    end
  end

  defp get_constructors(extensions, [], hint, arg_index) do
    Enum.flat_map(
      extensions,
      fn extension ->
        try do
          top_level_constructors =
            extension
            |> sections()
            |> Enum.filter(& &1.top_level?)
            |> Enum.flat_map(fn section ->
              do_find_constructors(section, [], hint, arg_index)
            end)

          extension.sections()
          |> apply_dsl_patches(extensions)
          |> Enum.filter(fn section ->
            !section.top_level? &&
              apply(@matcher, :match?, [to_string(section.name), hint])
          end)
          |> Enum.concat(top_level_constructors)
        rescue
          _e ->
            []
        end
      end
    )
  end

  defp get_constructors(extensions, [first | rest], hint, arg_index) do
    extensions
    |> Enum.flat_map(&sections/1)
    |> Enum.filter(& &1.top_level?)
    |> Enum.find(fn section ->
      Enum.any?(section.sections, &(&1.name == first)) ||
        Enum.any?(section.entities, &(&1.name == first))
    end)
    |> case do
      nil ->
        Enum.flat_map(
          extensions,
          fn extension ->
            try do
              extension
              |> sections()
              |> apply_dsl_patches(extensions)
              |> Enum.flat_map(fn section ->
                if section.name == first do
                  do_find_constructors(section, rest, hint, arg_index)
                else
                  []
                end
              end)
            rescue
              _e ->
                []
            end
          end
        )

      top_level_section ->
        do_find_constructors(top_level_section, [first | rest], hint, arg_index)
    end
  end

  defp apply_dsl_patches(sections_or_entities, extensions, path \\ [])

  defp apply_dsl_patches(sections_or_entities, extensions, path)
       when is_list(sections_or_entities) do
    Enum.map(sections_or_entities, &apply_dsl_patches(&1, extensions, path))
  end

  defp apply_dsl_patches(%Spark.Dsl.Entity{} = entity, _, _), do: entity

  defp apply_dsl_patches(%Spark.Dsl.Section{} = section, extensions, path) do
    section_path_matcher = path ++ [section.name]

    new_entities =
      Enum.flat_map(extensions, fn extension ->
        extension
        |> dsl_patches()
        |> Enum.flat_map(fn
          %Spark.Dsl.Patch.AddEntity{section_path: ^section_path_matcher, entity: entity} ->
            [entity]

          _ ->
            []
        end)
      end)

    %{
      section
      | sections:
          Enum.map(section.sections, &apply_dsl_patches(&1, extensions, path ++ [section.name])),
        entities: section.entities ++ new_entities
    }
  end

  defp do_find_constructors(entity_or_section, path, hint, arg_index, recursives \\ [])

  defp do_find_constructors(
         %{__struct__: Spark.Dsl.Entity} = entity,
         [],
         hint,
         arg_index,
         recursives
       ) do
    case argument_or_option(entity, arg_index) do
      {:argument, key, config} ->
        [{:value, key, config}]

      :option ->
        find_opt_hints(entity, hint) ++
          find_entity_hints(entity, hint, recursives)

      {:both, key, config} ->
        [{:value, key, config}] ++
          find_opt_hints(entity, hint) ++
          find_entity_hints(entity, hint, recursives)
    end
  end

  defp do_find_constructors(section, [], hint, _arg_index, recursives) do
    find_opt_hints(section, hint) ++
      find_entity_hints(section, hint, []) ++
      Enum.filter(section.sections, fn section ->
        apply(@matcher, :match?, [to_string(section.name), hint])
      end) ++ recursives
  end

  defp do_find_constructors(
         %{__struct__: Spark.Dsl.Entity} = entity,
         [next | rest],
         hint,
         arg_index,
         recursives
       ) do
    entity.entities
    |> Kernel.||([])
    |> Enum.flat_map(&elem(&1, 1))
    |> Enum.concat(recursives)
    |> Enum.concat(List.wrap(recursive_for(entity)))
    |> Enum.filter(&(&1.name == next))
    |> Enum.flat_map(
      &do_find_constructors(
        &1,
        rest,
        hint,
        arg_index,
        Enum.uniq(recursives ++ List.wrap(recursive_for(entity)))
      )
    )
    |> Enum.uniq()
  end

  defp do_find_constructors(section, [first | rest], hint, arg_index, recursives) do
    Enum.flat_map(section.entities, fn entity ->
      if entity.name == first do
        do_find_constructors(entity, rest, hint, arg_index)
      else
        []
      end
    end) ++
      Enum.flat_map(section.sections, fn section ->
        if section.name == first do
          do_find_constructors(section, rest, hint, arg_index)
        else
          []
        end
      end) ++ recursives
  end

  defp argument_or_option(entity, arg_index) do
    case Enum.at(entity.args || [], arg_index) do
      nil ->
        :option

      {:optional, key} ->
        {:both, key, entity.schema[key]}

      {:optional, key, _value} ->
        {:both, key, entity.schema[key]}

      key when is_atom(key) ->
        {:argument, key, entity.schema[key]}
    end
  end

  defp recursive_for(entity) do
    if entity.recursive_as do
      entity
    end
  end

  defp find_opt_hints(%{__struct__: Spark.Dsl.Entity} = entity, hint) do
    entity.schema
    |> Enum.reject(&(elem(&1, 0) in entity.args))
    |> Enum.flat_map(fn {key, value} ->
      if apply(@matcher, :match?, [to_string(key), hint]) do
        arg_index = Enum.find_index(Spark.Dsl.Entity.arg_names(entity), &(&1 == key))

        if arg_index do
          [{key, Keyword.put(value, :arg_index, arg_index)}]
        else
          [{key, value}]
        end
      else
        []
      end
    end)
  end

  defp find_opt_hints(section, hint) do
    Enum.filter(section.schema, fn {key, _value} ->
      apply(@matcher, :match?, [to_string(key), hint])
    end)
  end

  defp find_entity_hints(%{__struct__: Spark.Dsl.Entity} = entity, hint, recursives) do
    entity.entities
    |> Keyword.values()
    |> Enum.concat(recursives)
    |> Enum.concat(List.wrap(recursive_for(entity)))
    |> List.flatten()
    |> Enum.filter(fn entity ->
      apply(@matcher, :match?, [to_string(entity.name), hint])
    end)
  end

  defp find_entity_hints(section, hint, _recursives) do
    Enum.filter(section.entities, fn entity ->
      apply(@matcher, :match?, [to_string(entity.name), hint])
    end)
  end

  defp spark_extension?(module) do
    true in List.wrap(module.module_info()[:attributes][:spark_dsl])
  rescue
    _ -> false
  end

  defp default_extensions(dsl_mod) do
    dsl_mod.module_info()[:attributes][:spark_default_extensions]
    |> List.wrap()
    |> List.flatten()
  end

  defp parse_extensions(opts, extension_kinds) do
    Enum.flat_map([:extensions | extension_kinds], fn extension_kind ->
      case Regex.named_captures(
             ~r/#{extension_kind}:\s+?(?<extensions>(\[[^\]]*\])|([^\s]*))?[\,\s$]/,
             opts.cursor_context.text_before
           ) do
        %{"extensions" => extensions} when extensions != "" ->
          extensions
          |> String.replace("[", "")
          |> String.replace("]", "")
          |> String.split(",")
          |> Enum.map(&String.trim/1)

        _ ->
          []
      end
    end)
    |> Enum.uniq()
    |> Enum.map(&Module.concat([&1]))
    |> Enum.filter(fn module ->
      try do
        Code.ensure_loaded?(module)
      rescue
        _ ->
          false
      end
    end)
  end

  defp sections(extension) do
    extension.sections()
  rescue
    _ ->
      []
  end

  defp dsl_patches(extension) do
    extension.dsl_patches()
  rescue
    _ ->
      []
  end

  defp snippet_or_default(nil, default), do: default
  defp snippet_or_default("", default), do: default
  defp snippet_or_default(snippet, _default), do: snippet
end
