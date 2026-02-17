# SPDX-FileCopyrightText: 2022 spark contributors <https://github.com/ash-project/spark/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Spark.Dsl.Extension.Imports do
  @moduledoc false
  def import_solving_conflicts(mods, caller) do
    mods
    |> Enum.flat_map(fn mod ->
      [do_import(mod) | resolve_conflicts(mod, caller)]
    end)
  end

  defp resolve_conflicts(mod, caller) do
    imported_by_mod = mod.__info__(:functions) ++ mod.__info__(:macros)

    unimports_for_conflicts(caller, mod, imported_by_mod)
  end

  defp do_import(mod) do
    quote do
      import unquote(mod)
    end
  end

  defp unimports_for_conflicts(caller, importing_module, funs) do
    caller.functions
    |> Keyword.merge(caller.macros, fn _k, v1, v2 -> v1 ++ v2 end)
    |> Keyword.drop([Kernel, importing_module])
    |> Enum.flat_map(fn {mod, imports} ->
      imports
      |> Enum.filter(fn fun_arity ->
        fun_arity in funs
      end)
      |> case do
        [] ->
          []

        unimports ->
          [{mod, unimports}]
      end
    end)
    |> Enum.map(fn {mod, unimports} ->
      quote do
        import unquote(mod), except: unquote(unimports)
      end
    end)
  end
end
