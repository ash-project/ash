# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Signatures do
  @moduledoc false
  # The signatures of an operator or function, with their types resolved, as
  # `Ash.Expr.determine_types/4` matches them against operands.
  #
  # They only depend on the module's `types/0` (or `args/0`) and `returns/0`, so
  # `use Ash.Query.Operator` and `use Ash.Query.Function` work them out when the module is
  # compiled, as `__signatures__/0`, whenever those are defined as literals. Otherwise
  # `__signatures__/0` returns `:runtime`, and they are worked out when used. Operator
  # overloads declared by `:known_types` depend on the application, and are added at runtime.

  @type row ::
          {match_types :: list | atom, cast_as_types :: list | nil, returns :: term,
           declarer :: module | nil, declarer_positions :: term}

  @doc false
  # `:none` for a module that has no signatures to match (it takes `:var_args`, or its return
  # type is not known), otherwise its rows and whether its signatures are generic (see
  # `Ash.Expr.unsupported_signature?/3`).
  @spec own(term, term) :: :none | {[row], boolean}
  def own(types, returns) do
    if types == :var_args || returns == :no_return || returns == :unknown do
      :none
    else
      {rows(types, returns), generic?(types)}
    end
  end

  @doc false
  # Works out the signatures of `module` from its functions, as `__signatures__/0` would have.
  @spec from_module(module) :: :none | {[row], boolean}
  def from_module(module) do
    cond do
      :erlang.function_exported(module, :types, 0) -> own(module.types(), module.returns())
      :erlang.function_exported(module, :args, 0) -> own(module.args(), module.returns())
      true -> own([:any], [:any])
    end
  end

  @doc false
  def concrete_type?(vague) when vague in [:any, :same], do: false
  def concrete_type?({_, vague}) when vague in [:any, :same], do: false
  def concrete_type?(_), do: true

  @doc false
  def resolve_types(types) when is_list(types), do: Enum.map(types, &resolve_type/1)
  def resolve_types(types), do: types

  @doc false
  def resolve_return({:referenced_type, _via} = referenced), do: referenced
  def resolve_return(type), do: resolve_type(type)

  defp resolve_type({:array, any} = type) when any in [:same, :any], do: type
  defp resolve_type({type, any}) when any in [:same, :any], do: {Ash.Type.get_type(type), any}
  defp resolve_type(any) when any in [:same, :any], do: any
  defp resolve_type({type, constraints}), do: resolved(type, constraints)
  defp resolve_type(type), do: resolved(type, [])

  defp resolved(type, constraints) do
    if type = Ash.Type.get_type(type) do
      {type, constraints}
    end
  end

  defp generic?(types) do
    is_list(types) &&
      Enum.any?(types, fn
        :same -> true
        signature when is_list(signature) -> !Enum.any?(signature, &concrete_type?/1)
        _ -> false
      end)
  end

  defp rows([:any | rest_types], [_returns | rest_returns]), do: rows(rest_types, rest_returns)

  defp rows([types | rest_types], [returns | rest_returns]) do
    [
      {resolve_types(types), nil, resolve_return(returns), nil, nil}
      | rows(rest_types, rest_returns)
    ]
  end

  defp rows(_, _), do: []

  defmacro __before_compile__(env) do
    signatures =
      with {:ok, types} <- literal_types(env.module),
           {:ok, returns} <- literal(env.module, {:returns, 0}) do
        own(types, returns)
      else
        _ -> :runtime
      end

    quote do
      @doc false
      def __signatures__, do: unquote(Macro.escape(signatures))
    end
  end

  defp literal_types(module) do
    cond do
      Module.defines?(module, {:types, 0}, :def) -> literal(module, {:types, 0})
      Module.defines?(module, {:args, 0}, :def) -> literal(module, {:args, 0})
      true -> :error
    end
  end

  # The value of a function defined with a single clause whose body is a literal. Module
  # attributes in it have been expanded by now.
  # sobelow_skip ["RCE.CodeModule"]
  defp literal(module, fun) do
    case Module.get_definition(module, fun) do
      {:v1, :def, _meta, [{_clause_meta, [], [], body}]} ->
        if Macro.quoted_literal?(body) do
          {value, _binding} = Code.eval_quoted(body)
          {:ok, value}
        else
          :error
        end

      _ ->
        :error
    end
  end
end
