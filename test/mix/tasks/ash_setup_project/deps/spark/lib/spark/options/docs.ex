# SPDX-FileCopyrightText: 2022 spark contributors <https://github.com/ash-project/spark/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Spark.Options.Docs do
  @moduledoc false

  def generate(schema, options) when is_list(schema) and is_list(options) do
    nest_level = Keyword.get(options, :nest_level, 0)
    {docs, sections, _level} = build_docs(schema, {[], [], nest_level})
    to_string([Enum.reverse(docs), Enum.reverse(sections)])
  end

  # If the schema is a function, we want to not show anything (it's a recursive
  # function) and "back up" one level since when we got here we already
  # increased the level by one.
  defp build_docs(fun, {docs, sections, level}) when is_function(fun) do
    {docs, sections, level - 1}
  end

  defp build_docs(schema, {docs, sections, level} = acc) do
    if schema[:*] do
      build_docs(schema[:*][:keys], acc)
    else
      Enum.reduce(schema || [], {docs, sections, level}, &maybe_option_doc/2)
    end
  end

  defp build_docs_with_subsection(subsection, schema, {docs, sections, level}) do
    subsection = String.trim_trailing(subsection, "\n") <> "\n\n"

    {item_docs, sections, _level} = build_docs(schema, {[], sections, 0})
    item_section = [subsection | Enum.reverse(item_docs)]

    {docs, [item_section | sections], level}
  end

  defp maybe_option_doc({key, schema}, acc) do
    if schema[:doc] == false do
      acc
    else
      option_doc({key, schema}, acc)
    end
  end

  defp option_doc({key, schema}, {docs, sections, level}) do
    type_str = if type_str = get_type_str(schema), do: " #{type_str}"
    desc_indent = String.duplicate("  ", level + 1)

    description =
      get_required_str(schema)
      |> get_doc_str(schema)
      |> get_default_str(schema)
      |> case do
        nil -> ""
        parts -> indent_doc(" - " <> parts, desc_indent)
      end

    doc = [String.duplicate("  ", level), "* `#{inspect(key)}`#{type_str}", description, "\n\n"]
    docs = [IO.iodata_to_binary(doc) | docs]

    cond do
      schema[:keys] && schema[:subsection] ->
        build_docs_with_subsection(schema[:subsection], schema[:keys], {docs, sections, level})

      schema[:keys] ->
        {docs, sections, _level} = build_docs(schema[:keys], {docs, sections, level + 1})
        {docs, sections, level}

      true ->
        {docs, sections, level}
    end
  end

  defp space_concat(left, nil), do: left
  defp space_concat(nil, right), do: right
  defp space_concat(left, right), do: left <> " " <> right

  defp get_required_str(schema) do
    if schema[:required], do: "Required."
  end

  defp get_doc_str(prev_str, schema) do
    space_concat(prev_str, schema[:doc] && String.trim(schema[:doc]))
  end

  defp get_default_str(prev_str, schema) do
    if Keyword.has_key?(schema, :default) do
      default_str = "The default value is `#{inspect(schema[:default])}`."

      # If the documentation contains multiple lines,
      # the default must be in a trailing line.
      if prev_str && String.contains?(prev_str, ["\r\n", "\n\n"]) do
        prev_str <> "\n\n" <> default_str
      else
        space_concat(prev_str, default_str)
      end
    else
      prev_str
    end
  end

  defp get_type_str(schema) do
    str =
      case Keyword.fetch(schema, :type_doc) do
        {:ok, false} -> nil
        {:ok, type_doc} when is_binary(type_doc) -> type_doc
        :error -> get_raw_type_str(schema[:type])
      end

    if str do
      "(#{str})"
    end
  end

  # Only shows types when they are concise.
  defp get_raw_type_str(nil), do: nil
  defp get_raw_type_str({:custom, _mod, _fun, _args}), do: nil
  defp get_raw_type_str(:mfa), do: nil
  defp get_raw_type_str(:mod_arg), do: nil
  defp get_raw_type_str(:module), do: "`t:module/0`"
  defp get_raw_type_str({:or, _values}), do: nil
  defp get_raw_type_str({:in, _}), do: nil
  defp get_raw_type_str({:one_of, _}), do: nil
  defp get_raw_type_str({:fun, arity}) when is_integer(arity), do: "function of arity #{arity}"
  defp get_raw_type_str(:any), do: "`t:term/0`"
  defp get_raw_type_str(:reference), do: "`t:reference/0`"
  defp get_raw_type_str(:pid), do: "`t:pid/0`"
  defp get_raw_type_str(:timeout), do: "`t:timeout/0`"
  defp get_raw_type_str(:boolean), do: "`t:boolean/0`"
  defp get_raw_type_str(:atom), do: "`t:atom/0`"
  defp get_raw_type_str(:integer), do: "`t:integer/0`"
  defp get_raw_type_str(:regex_as_mfa), do: "`t:Regex.t/0`"
  defp get_raw_type_str(:regex), do: "`t:Regex.t/0`"
  defp get_raw_type_str(:non_neg_integer), do: "`t:non_neg_integer/0`"
  defp get_raw_type_str(:pos_integer), do: "`t:pos_integer/0`"
  defp get_raw_type_str(:float), do: "`t:float/0`"
  defp get_raw_type_str(:number), do: "`t:number/0`"
  defp get_raw_type_str(:string), do: "`t:String.t/0`"
  defp get_raw_type_str(:keyword_list), do: "`t:keyword/0`"
  defp get_raw_type_str(:non_empty_keyword_list), do: "non-empty `t:keyword/0`"
  defp get_raw_type_str({:map, _keys}), do: "`t:map/0`"
  defp get_raw_type_str({:keyword_list, _keys}), do: "`t:keyword/0`"
  defp get_raw_type_str({:non_empty_keyword_list, _keys}), do: "non-empty `t:keyword/0`"
  defp get_raw_type_str(:map), do: "`t:map/0`"

  defp get_raw_type_str({:tagged_tuple, tag, tuple_type}) do
    get_raw_type_str({:tuple, [{:literal, tag}, tuple_type]})
  end

  defp get_raw_type_str({:spark_behaviour, module}),
    do: "module that adopts `#{inspect(module)}` or a module and options"

  defp get_raw_type_str({:spark_behaviour, module, _}),
    do: "module that adopts `#{inspect(module)}` or a module and options"

  defp get_raw_type_str({:spark_function_behaviour, module, {_, integer}}),
    do:
      "module that adopts `#{inspect(module)}`, a module and options, or a function of arity #{integer}"

  defp get_raw_type_str({:spark_function_behaviour, module, _, {_, integer}}),
    do:
      "module that adopts `#{inspect(module)}`, a module and options, or a function of arity #{integer}"

  defp get_raw_type_str({:behaviour, module}), do: "module that adopts `#{inspect(module)}`"

  defp get_raw_type_str({:protocol, module}),
    do: "value that implements the `#{inspect(module)}` protocol"

  defp get_raw_type_str({:impl, module}),
    do: "module for which the `#{inspect(module)}` protocol is implemented"

  defp get_raw_type_str({:struct, struct_type}), do: "struct of type `#{inspect(struct_type)}`"
  defp get_raw_type_str(:struct), do: "struct"
  defp get_raw_type_str({:spark, module}), do: "`#{inspect(module)}`"
  defp get_raw_type_str({:mfa_or_fun, arity}), do: "mfa or function of arity #{arity}"
  defp get_raw_type_str({:spark_type, module, _}), do: "an `#{inspect(module)}`"
  defp get_raw_type_str({:spark_type, module, _, _}), do: "an `#{inspect(module)}`"
  defp get_raw_type_str(:fun), do: "function"
  defp get_raw_type_str({:fun, _}), do: "function"
  defp get_raw_type_str({:fun, _, _}), do: "function"

  defp get_raw_type_str({:wrap_list, subtype}) do
    if subtype_str = get_raw_type_str(subtype), do: "one or a list of #{subtype_str}"
  end

  defp get_raw_type_str(:literal), do: nil
  defp get_raw_type_str({:literal, value}), do: "`#{inspect(value)}`"
  defp get_raw_type_str(:quoted), do: "a quoted value"

  defp get_raw_type_str({:list, subtype}) do
    if subtype_str = get_raw_type_str(subtype), do: "list of #{subtype_str}"
  end

  defp get_raw_type_str({:map, key_type, value_type}) do
    with key_type_str when is_binary(key_type_str) <- get_raw_type_str(key_type),
         value_type_str when is_binary(value_type_str) <- get_raw_type_str(value_type) do
      "map of #{key_type_str} keys and #{value_type_str} values"
    end
  end

  defp get_raw_type_str({:tuple, value_types}) do
    value_types =
      value_types
      |> Enum.map_join(", ", &get_raw_type_str/1)

    "tuple of #{value_types} values"
  end

  def dsl_docs_type({:behaviour, _mod}), do: "module"
  def dsl_docs_type({:protocol, protocol}), do: "an `#{inspect(protocol)}` value"
  def dsl_docs_type({:impl, protocol}), do: "a module implementing `#{inspect(protocol)}`"
  def dsl_docs_type({:spark, mod}), do: dsl_docs_type({:behaviour, mod})
  def dsl_docs_type({:spark_behaviour, mod}), do: dsl_docs_type({:behaviour, mod})

  def dsl_docs_type({:spark_behaviour, mod, _builtins}),
    do: dsl_docs_type({:behaviour, inspect(mod)})

  def dsl_docs_type(:module), do: "module"

  def dsl_docs_type({:spark_function_behaviour, mod, {_, arity}}) do
    dsl_docs_type({:or, [{:fun, arity}, {:spark_behaviour, mod}]})
  end

  def dsl_docs_type({:spark_function_behaviour, mod, _builtins, {_, arity}}) do
    dsl_docs_type({:or, [{:fun, arity}, {:spark_behaviour, mod}]})
  end

  def dsl_docs_type({:custom, _, _, _}), do: "any"
  def dsl_docs_type(:any), do: "any"

  def dsl_docs_type(:keyword_list), do: "keyword"
  def dsl_docs_type(:non_empty_keyword_list), do: "keyword"

  def dsl_docs_type({keyword_type, [*: value]})
      when keyword_type in [:keyword_list, :non_empty_keyword_list] and is_list(value),
      do: "keyword(#{dsl_docs_type(value[:type])})"

  def dsl_docs_type({keyword_type, schema})
      when keyword_type in [:keyword_list, :non_empty_keyword_list] and is_list(schema) do
    options =
      Enum.map_join(schema, ", ", fn {key, value} ->
        "#{key}: #{dsl_docs_type(value[:type])}"
      end)

    "[#{options}]"
  end

  def dsl_docs_type(:atom), do: "atom"
  def dsl_docs_type(:string), do: "String.t"
  def dsl_docs_type(:boolean), do: "boolean"
  def dsl_docs_type(:integer), do: "integer"
  def dsl_docs_type(:non_neg_integer), do: "non_neg_integer"
  def dsl_docs_type(:pos_integer), do: "pos_integer"
  def dsl_docs_type(:float), do: "float"
  def dsl_docs_type(:number), do: "number"
  def dsl_docs_type(:timeout), do: "timeout"
  def dsl_docs_type(:pid), do: "pid"
  def dsl_docs_type(:mfa), do: "mfa"
  def dsl_docs_type(:mod_arg), do: dsl_docs_type({:tuple, [:module, {:list, :any}]})

  def dsl_docs_type({:tuple, list}) when is_list(list) do
    tuple = Enum.map_join(list, ", ", &dsl_docs_type/1)

    "{#{tuple}}"
  end

  def dsl_docs_type(:map), do: "map"

  def dsl_docs_type({:map, key_type, value_type}),
    do: "%{optional(#{dsl_docs_type(key_type)}) => #{dsl_docs_type(value_type)}}"

  def dsl_docs_type({:map, schema}) when is_list(schema) do
    options =
      Enum.map_join(schema, ", ", fn {key, value} ->
        "#{if value[:required], do: "required", else: "optional"}(:#{key}) => #{dsl_docs_type(value[:type])}"
      end)

    "%{#{options}}"
  end

  def dsl_docs_type(:struct), do: "struct"
  def dsl_docs_type({:struct, struct}), do: "#{inspect(struct)}"
  def dsl_docs_type(nil), do: "nil"
  def dsl_docs_type({:fun, 0}), do: "(-> any)"

  def dsl_docs_type({:fun, arity}) when is_integer(arity) do
    args =
      Enum.map_join(0..(arity - 1), ", ", fn _ -> "any" end)

    "(#{args} -> any)"
  end

  def dsl_docs_type({:fun, args}) when is_list(args) do
    args =
      Enum.map_join(args, ", ", &dsl_docs_type/1)

    "(#{args} -> any)"
  end

  def dsl_docs_type({:fun, args, return}) when is_list(args) do
    args =
      Enum.map_join(args, ", ", &dsl_docs_type/1)

    "(#{args} -> #{dsl_docs_type(return)})"
  end

  def dsl_docs_type({:one_of, values}), do: dsl_docs_type({:in, values})
  def dsl_docs_type({:in, values}), do: Enum.map_join(values, " | ", &inspect/1)

  def dsl_docs_type({:or, subtypes}),
    do: subtypes |> Enum.map(&dsl_docs_type/1) |> Enum.uniq() |> Enum.join(" | ")

  def dsl_docs_type({:list, subtype}), do: "list(#{dsl_docs_type(subtype)})"
  def dsl_docs_type(:quoted), do: "any"

  def dsl_docs_type({:wrap_list, subtype}) do
    str = dsl_docs_type(subtype)
    "#{str} | list(#{str})"
  end

  def dsl_docs_type({:list_of, subtype}), do: dsl_docs_type({:list, subtype})
  def dsl_docs_type({:mfa_or_fun, arity}), do: dsl_docs_type({:or, [{:fun, arity}, :mfa]})
  def dsl_docs_type(:literal), do: "any"
  def dsl_docs_type({:literal, value}), do: inspect(value)

  def dsl_docs_type({:tagged_tuple, tag, type}),
    do: dsl_docs_type({:tuple, [{:literal, tag}, type]})

  def dsl_docs_type({:spark_type, type, _}), do: dsl_docs_type({:behaviour, type})
  def dsl_docs_type({:spark_type, type, _, _}), do: dsl_docs_type({:behaviour, type})

  defp indent_doc(text, indent) do
    [head | tail] = String.split(text, ["\r\n", "\n"])

    tail =
      Enum.map(tail, fn
        "" -> "\n"
        str -> [?\n, indent, str]
      end)

    [head | tail]
  end

  def schema_specs(schema, or_nil? \\ false) do
    Enum.map(schema, fn {key, opt_schema} ->
      typespec =
        Keyword.get_lazy(opt_schema, :type_spec, fn -> type_to_spec(opt_schema[:type]) end)

      if or_nil? && !opt_schema[:required] && is_nil(opt_schema[:default]) do
        quote do: {unquote(key), unquote(typespec) | nil}
      else
        quote do: {unquote(key), unquote(typespec)}
      end
    end)
  end

  def schema_to_spec(schema) do
    schema
    |> schema_specs()
    |> unionize_quoted()
  end

  defp type_to_spec(type) do
    case type do
      :any ->
        quote(do: term())

      :integer ->
        quote(do: integer())

      :non_neg_integer ->
        quote(do: non_neg_integer())

      :pos_integer ->
        quote(do: pos_integer())

      :atom ->
        quote(do: atom())

      :float ->
        quote(do: float())

      :boolean ->
        quote(do: boolean())

      :pid ->
        quote(do: pid())

      :reference ->
        quote(do: reference())

      :timeout ->
        quote(do: timeout())

      :string ->
        quote(do: String.t())

      :mfa ->
        quote(do: {module(), atom(), [term()]})

      :mod_arg ->
        quote(do: {module(), [term()]})

      :keyword_list ->
        quote(do: keyword())

      {:keyword_list, _keys} ->
        quote(do: keyword())

      :non_empty_keyword_list ->
        quote(do: keyword())

      {:non_empty_keyword_list, _keys} ->
        quote(do: keyword())

      nil ->
        quote(do: nil)

      :regex_as_mfa ->
        quote(do: Regex.t())

      :regex ->
        quote(do: Regex.t())

      :map ->
        quote(do: map())

      {:map, key_type, value_type} ->
        quote(
          do: %{optional(unquote(type_to_spec(key_type))) => unquote(type_to_spec(value_type))}
        )

      :fun ->
        quote do: (... -> term())

      {:fun, arity} when is_integer(arity) ->
        function_spec(arity)

      {:fun, arg_types} ->
        function_spec(arg_types)

      {:fun, arg_types, return_type} ->
        function_spec(arg_types, return_type)

      {:in, %Range{first: first, last: last} = range} ->
        if Map.get(range, :step) in [nil, 1] do
          quote(do: unquote(first)..unquote(last))
        else
          quote(do: term())
        end

      {one_of, choices} when one_of in [:in, :one_of] ->
        if !Enum.empty?(choices) && Enum.all?(choices, &is_atom(&1)) do
          unionize_quoted(choices)
        else
          quote(do: term())
        end

      {:custom, _mod, _fun, _args} ->
        quote(do: term())

      {:list, subtype} ->
        quote(do: [unquote(type_to_spec(subtype))])

      {:or, subtypes} ->
        subtypes |> Enum.map(&type_to_spec/1) |> unionize_quoted()

      {:struct, _struct_name} ->
        quote(do: struct())

      :struct ->
        quote(do: struct())

      {:tuple, tuple_types} ->
        case Enum.map(tuple_types, &type_to_spec/1) do
          [type1, type2] -> quote(do: {unquote(type1), unquote(type2)})
          types -> quote do: {unquote_splicing(types)}
        end

      {:tagged_tuple, tag, subtype} ->
        quote do: {unquote(tag), unquote(type_to_spec(subtype))}

      {tag, _} when tag in [:behaviour, :spark_behaviour, :protocol, :spark, :impl] ->
        quote(do: module())

      {:spark_function_behaviour, _, _} ->
        quote(do: module() | {module(), Keyword.t()})

      {:spark_function_behaviour, _, _, _} ->
        quote(do: module() | {module(), Keyword.t()})

      {:spark_type, _, _} ->
        quote(do: module)

      {:spark_type, _, _, _} ->
        quote(do: module)

      {:mfa_or_fun, arity} ->
        quote do: unquote(type_to_spec(:mfa)) | unquote(type_to_spec({:fun, arity}))

      {:struct, module} ->
        quote do: unquote(module).t()

      {:wrap_list, type} ->
        quote do: unquote(type_to_spec(type)) | [unquote(type_to_spec(type))]

      :literal ->
        quote do: term()

      {:literal, atom} when is_atom(atom) ->
        atom

      {:literal, _} ->
        quote do: term()

      :quoted ->
        quote do: Macro.t()
    end
  end

  defp function_spec(arity) when is_integer(arity) do
    args = List.duplicate(quote(do: term()), arity)

    quote do
      unquote_splicing(args) -> term()
    end
  end

  defp function_spec(arg_types) when is_list(arg_types) do
    arg_types = arg_types |> Enum.map(&type_to_spec/1)

    quote do
      unquote_splicing(arg_types) -> term()
    end
  end

  defp function_spec(arg_types, return_type) when is_list(arg_types) do
    arg_types = arg_types |> Enum.map(&type_to_spec/1)
    return_type = type_to_spec(return_type)

    quote do
      unquote_splicing(arg_types) -> unquote(return_type)
    end
  end

  defp unionize_quoted(specs) do
    specs
    |> Enum.reverse()
    |> Enum.uniq_by(&remove_meta/1)
    |> Enum.reduce(&quote(do: unquote(&1) | unquote(&2)))
  end

  defp remove_meta(code) do
    Macro.prewalk(code, fn
      {a, _b, c} ->
        {a, [], c}

      other ->
        other
    end)
  end
end
