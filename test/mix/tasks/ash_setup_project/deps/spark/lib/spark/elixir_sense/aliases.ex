# SPDX-FileCopyrightText: 2022 spark contributors <https://github.com/ash-project/spark/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Spark.ElixirSense.Types do
  @moduledoc false

  alias ElixirSense.Core.Introspection

  cond do
    Code.ensure_loaded?(ElixirSense.Providers.Plugins.Util) ->
      @util ElixirSense.Providers.Plugins.Util

    Code.ensure_loaded?(ElixirLS.LanguageServer.Plugins.Util) ->
      @util ElixirLS.LanguageServer.Plugins.Util

    true ->
      @util ElixirSense.Plugins.Util
  end

  if Code.ensure_loaded?(ElixirLS.Utils.Matcher) do
    @matcher ElixirLS.Utils.Matcher
  else
    @matcher ElixirSense.Providers.Suggestion.Matcher
  end

  def find_builtin_types(module, func, hint, cursor_context, templates \\ []) do
    text_before = cursor_context.text_before
    text_after = cursor_context.text_after

    actual_hint =
      if String.ends_with?(text_before, "{" <> hint) do
        "{" <> hint
      else
        hint
      end

    with true <- Code.ensure_loaded?(module),
         true <- :erlang.function_exported(module, func, 0) do
      for {name, _, _} = type <- builtin_types(module, func, templates),
          apply(@matcher, :match?, [name, actual_hint]) do
        buitin_type_to_suggestion(type, module, actual_hint, text_after)
      end
    end
  end

  defp builtin_types(module, func, templates) do
    module
    |> apply(func, [])
    |> Enum.map(fn {name, value} ->
      {inspect(name), value, nil}
    end)
    |> Enum.concat(templates)
  end

  def find_custom_types(type_module, func, hint, module_store) do
    builtin_types = Keyword.values(apply(type_module, func, []))

    for module <- module_store.by_behaviour[type_module] || [],
        module not in builtin_types,
        type_str = inspect(module),
        apply(@util, :match_module?, [type_str, hint]) do
      custom_type_to_suggestion(module, type_module, hint)
    end
  end

  defp buitin_type_to_suggestion({type, mod_or_description, snippet}, module, hint, text_after) do
    [_, hint_prefix] = Regex.run(~r/(.*?)[\w0-9\._!\?\->]*$/, hint)

    insert_text = String.replace_prefix(type, hint_prefix, "")
    snippet = snippet && String.replace_prefix(snippet, hint_prefix, "")

    {insert_text, snippet} =
      if String.starts_with?(text_after, "}") do
        snippet = snippet && String.replace_suffix(snippet, "}", "")
        insert_text = String.replace_suffix(insert_text, "}", "")
        {insert_text, snippet}
      else
        {insert_text, snippet}
      end

    docs =
      if is_binary(mod_or_description) do
        mod_or_description
      else
        {doc, _} = apply(Introspection, :get_module_docs_summary, [mod_or_description])
        doc
      end

    doc = """
    Built-in #{inspect(module)}

    #{docs}
    """

    %{
      type: :generic,
      kind: :type_parameter,
      label: type,
      insert_text: insert_text,
      snippet: snippet,
      detail: inspect(module),
      documentation: doc,
      priority: 0
    }
  end

  defp custom_type_to_suggestion(type, module, hint) do
    type_str = inspect(type)
    {doc, _} = apply(Introspection, :get_module_docs_summary, [type])

    %{
      type: :generic,
      kind: :type_parameter,
      label: type_str,
      insert_text: apply(Util, :trim_leading_for_insertion, [hint, type_str]),
      detail: "Custom #{inspect(module)}",
      documentation: doc,
      priority: 1
    }
  end
end
