# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.BehaviourHelpers do
  @moduledoc false

  defmacro check_type!(module, result, patterns) do
    {function, arity} = __CALLER__.function

    raise =
      if Enum.count_until(patterns, 2) == 2 do
        pattern_string =
          Enum.map_join(patterns, "\n", fn pattern ->
            "* " <> Macro.to_string(pattern)
          end)

        quote do
          raise Ash.Error.Framework.InvalidReturnType,
            message: """
            Invalid value returned from `#{inspect(unquote(module))}.#{unquote(function)}/#{unquote(arity - 1)}`.

            The callback `#{inspect(unquote(__CALLER__.module))}.#{unquote(function)}/#{unquote(arity - 1)}` expects one of the following return types:

            #{unquote(pattern_string)}
            """
        end
      else
        quote do
          raise Ash.Error.Framework.InvalidReturnType,
            message: """
            Invalid value returned from `#{inspect(unquote(module))}.#{unquote(function)}/#{unquote(arity - 1)}`.

            The callback `#{inspect(unquote(__CALLER__.module))}.#{unquote(function)}/#{unquote(arity - 1)}` expects the following return:

            #{unquote(Macro.to_string(Enum.at(patterns, 0)))}
            """
        end
      end

    result_var = Macro.var(:result, __MODULE__)

    matches =
      Enum.map(patterns, fn pattern ->
        {:->, [generated: true], [[pattern], result_var]}
      end)
      |> Enum.concat([
        {:->, [generated: true], [[{:_, [generated: true], Elixir}], raise]}
      ])

    case =
      {:case, [generated: true], [result_var, [do: matches]]}
      |> Macro.prewalk(fn
        {x, meta, y} ->
          {x, Keyword.put(meta, :generated, true), y}

        other ->
          other
      end)

    quote generated: true do
      unquote(result_var) = unquote(result)
      unquote(case)
    end
  end
end
