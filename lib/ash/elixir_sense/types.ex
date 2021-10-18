if Code.ensure_loaded?(ElixirSense) do
  defmodule Ash.ElixirSense.Types do
    @moduledoc false

    alias ElixirSense.Core.Introspection
    alias ElixirSense.Plugins.Util
    alias ElixirSense.Providers.Suggestion.Matcher

    def find_builtin_types(hint, cursor_context) do
      text_before = cursor_context.text_before
      text_after = cursor_context.text_after

      actual_hint =
        if String.ends_with?(text_before, "{" <> hint) do
          "{" <> hint
        else
          hint
        end

      with true <- Code.ensure_loaded?(Ash.Type),
           true <- :erlang.function_exported(Ash.Type, :builtin_types, 0) do
        for {name, _, _} = type <- builtin_types(),
            Matcher.match?(name, actual_hint) do
          buitin_type_to_suggestion(type, actual_hint, text_after)
        end
      end
    end

    defp builtin_types() do
      Ash.Type
      |> apply(:builtin_types, [])
      |> Enum.map(fn {name, value} ->
        {inspect(name), value, nil}
      end)
      |> Enum.concat([{"{:array, inner_type}", "list", "{:array, ${1:inner_type}}"}])
    end

    def find_custom_types(hint) do
      builtin_types = Keyword.values(apply(Ash.Type, :builtin_types, []))

      for {module, _} <- :code.all_loaded(),
          Ash.Type in behaviours(module),
          module not in builtin_types,
          type_str = inspect(module),
          Util.match_module?(type_str, hint) do
        custom_type_to_suggestion(module, hint)
      end
    end

    defp behaviours(module) do
      module.module_info(:attributes)[:behaviour] || []
    rescue
      _ ->
        []
    end

    defp buitin_type_to_suggestion({type, mod_or_description, snippet}, hint, text_after) do
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
          {doc, _} = Introspection.get_module_docs_summary(mod_or_description)
          doc
        end

      doc = """
      Built-in Ash type

      #{docs}
      """

      %{
        type: :generic,
        kind: :type_parameter,
        label: type,
        insert_text: insert_text,
        snippet: snippet,
        detail: "Ash type",
        documentation: doc,
        priority: 0
      }
    end

    defp custom_type_to_suggestion(type, hint) do
      type_str = inspect(type)
      {doc, _} = Introspection.get_module_docs_summary(type)

      %{
        type: :generic,
        kind: :type_parameter,
        label: type_str,
        insert_text: Util.trim_leading_for_insertion(hint, type_str),
        detail: "Ash custom type",
        documentation: doc,
        priority: 1
      }
    end
  end
end
