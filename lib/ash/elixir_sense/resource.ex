if Code.ensure_loaded?(ElixirSense.Plugin) do
  defmodule Ash.ElixirSense.Resource do
    @moduledoc false
    alias ElixirSense.Core.Introspection
    alias ElixirSense.Plugins.Util
    alias ElixirSense.Providers.Suggestion.Complete

    def find_resources(hint) do
      for {module, _} <- :code.all_loaded(),
          Ash.Resource in (module.module_info(:attributes)[:ash_is] || []),
          mod_str = inspect(module),
          Util.match_module?(mod_str, hint) do
        {doc, _} = Introspection.get_module_docs_summary(module)

        %{
          type: :generic,
          kind: :class,
          label: mod_str,
          insert_text: Util.trim_leading_for_insertion(hint, mod_str),
          detail: "Ash resource",
          documentation: doc
        }
      end
    end

    def find_ash_behaviour_impls(behaviour, builtins, hint, module_store) do
      builtins =
        if builtins && !String.contains?(hint, ".") && lowercase_string?(hint) do
          Complete.complete(to_string("#{inspect(builtins)}.#{hint}"), %Complete.Env{})
        else
          []
        end

      custom =
        for module <- module_store.by_behaviour[behaviour],
            mod_str = inspect(module),
            !String.starts_with?(mod_str, "Ash."),
            Util.match_module?(mod_str, hint) do
          {doc, _} = Introspection.get_module_docs_summary(module)

          %{
            type: :generic,
            kind: :class,
            label: mod_str,
            insert_text: Util.trim_leading_for_insertion(hint, mod_str),
            detail: "#{inspect(behaviour)}",
            documentation: doc
          }
        end

      builtins ++ custom
    end

    defp lowercase_string?(string) do
      first = String.first(string)
      String.downcase(first) == first
    end
  end
end
