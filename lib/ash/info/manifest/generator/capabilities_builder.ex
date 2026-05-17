# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Info.Manifest.Generator.CapabilitiesBuilder do
  @moduledoc """
  Builds the top-level `%FilterCapabilities{}` and `%SortCapabilities{}` for a
  manifest by enumerating Ash's builtin operators/functions and the app's
  registered custom expressions.
  """

  alias Ash.Info.Manifest.{
    ArgumentSignature,
    CustomExpression,
    FilterCapabilities,
    Function,
    Operator,
    SortCapabilities
  }

  @doc """
  Build the global `%FilterCapabilities{}` and `%SortCapabilities{}` for a
  manifest.

  ## Options

    * `:data_layer_modules` - List of data-layer modules whose `functions/1`
      callback contributes additional predicate functions to the catalog. Each
      contributed `%Function{}` is tagged with `data_layer_module` so consumers
      can intersect against a resource's data layer.

  Without `:data_layer_modules`, the catalog is the Ash builtins plus the
  app's registered custom expressions — identical to the previous `build/0`
  behavior.
  """
  @spec build(keyword()) :: {FilterCapabilities.t(), SortCapabilities.t()}
  def build(opts \\ []) do
    data_layer_modules =
      opts
      |> Keyword.get(:data_layer_modules, [])
      |> Enum.uniq()

    operators = Enum.map(Ash.Filter.builtin_operators(), &build_operator/1)

    builtin_functions = Enum.map(Ash.Filter.builtin_functions(), &build_function(&1, nil))
    data_layer_functions = collect_data_layer_functions(data_layer_modules)
    functions = builtin_functions ++ data_layer_functions

    custom_expressions =
      :ash
      |> Application.get_env(:custom_expressions, [])
      |> Enum.map(&build_custom_expression/1)

    filter_caps = %FilterCapabilities{
      operators: operators,
      functions: functions,
      custom_expressions: custom_expressions,
      boolean_connectives: [:and, :or, :not],
      predicate_operators: predicate_names(operators),
      predicate_functions: predicate_names(functions),
      predicate_custom_expressions: predicate_names(custom_expressions)
    }

    {filter_caps, %SortCapabilities{}}
  end

  # Each data-layer module contributes its `functions/1` modules, deduplicated
  # by function module across data layers. Tag with the *first* data layer that
  # claimed it. Modules that don't export `functions/1` contribute nothing.
  defp collect_data_layer_functions(data_layer_modules) do
    Enum.flat_map(data_layer_modules, fn dl_module ->
      Code.ensure_loaded(dl_module)

      if function_exported?(dl_module, :functions, 1) do
        # The functions/1 callback is per-resource in the Ash data-layer API,
        # but at this catalog level we just need the set of contributed
        # modules. Passing nil works for data layers whose `functions/1` is
        # resource-independent (Postgres ilike/trigram, the test fake).
        apply(dl_module, :functions, [nil])
      else
        []
      end
    end)
    |> Enum.uniq()
    |> Enum.map(fn module ->
      data_layer_module = first_data_layer_owning(module, data_layer_modules)
      build_function(module, data_layer_module)
    end)
  end

  defp first_data_layer_owning(function_module, data_layer_modules) do
    Enum.find(data_layer_modules, fn dl ->
      Code.ensure_loaded(dl)

      function_exported?(dl, :functions, 1) and
        function_module in apply(dl, :functions, [nil])
    end)
  end

  defp build_operator(module) do
    Code.ensure_loaded(module)
    {name, aliases} = operator_name_and_aliases(module)

    %Operator{
      name: name,
      module: module,
      aliases: aliases,
      predicate?: module.predicate?(),
      signatures: normalize_signatures(module.types()),
      returns: normalize_returns(module.returns()),
      description: module_doc(module)
    }
  end

  defp build_function(module, data_layer_module) do
    Code.ensure_loaded(module)

    signatures =
      case module.args() do
        :var_args -> :var_args
        args -> normalize_signatures(args)
      end

    %Function{
      name: module.name(),
      module: module,
      predicate?: module.predicate?(),
      signatures: signatures,
      returns: normalize_returns(module.returns()),
      description: module_doc(module),
      data_layer_module: data_layer_module
    }
  end

  defp build_custom_expression(module) do
    %CustomExpression{
      name: module.name(),
      module: module,
      predicate?: module.predicate?(),
      signatures: normalize_signatures(module.arguments()),
      description: module_doc(module)
    }
  end

  # Canonical name = the user-facing symbol (`:==`, `:<`, `:in`, `:is_nil`).
  # For operators where `name:` differs from `operator:` (e.g. Eq has
  # operator: :==, name: :eq), the operator symbol is what users write in
  # `Ash.Query.filter` expressions and what extensions render to clients.
  defp operator_name_and_aliases(module) do
    if function_exported?(module, :operator, 0) do
      op = module.operator()
      internal_name = module.name()
      aliases = if internal_name != op, do: [internal_name], else: []
      {op, aliases}
    else
      {module.name(), []}
    end
  end

  defp normalize_signatures(signatures) when is_list(signatures) do
    Enum.map(signatures, &ArgumentSignature.from_ash_signature/1)
  end

  defp normalize_signatures(_), do: []

  defp normalize_returns(:unknown), do: :unknown
  defp normalize_returns(nil), do: :unknown

  defp normalize_returns(returns) when is_list(returns) do
    case returns do
      [] -> :unknown
      [single] -> ArgumentSignature.normalize_arg(single)
      [first | _] -> ArgumentSignature.normalize_arg(first)
    end
  end

  defp normalize_returns(other) when is_atom(other) or is_tuple(other) do
    ArgumentSignature.normalize_arg(other)
  end

  defp normalize_returns(_), do: :unknown

  defp predicate_names(entries) do
    entries
    |> Enum.filter(& &1.predicate?)
    |> Enum.map(& &1.name)
  end

  defp module_doc(module) do
    case Code.fetch_docs(module) do
      {:docs_v1, _, _, _, %{"en" => doc}, _, _} -> doc
      _ -> nil
    end
  rescue
    _ -> nil
  end
end
