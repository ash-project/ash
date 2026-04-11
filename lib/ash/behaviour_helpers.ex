# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.BehaviourHelpers do
  @moduledoc """
  Helpers for Ash behaviour modules: return-type validation and documentation of the wrapper pattern.

  ## Behaviour invocation pattern

  Implementation modules (e.g. a module that `@behaviour Ash.Resource.Change`) should be invoked
  **only through the behaviour module's public wrapper callbacks**. Call sites must call
  `SomeBehaviour.callback(implementation_module, ...)` rather than
  `implementation_module.callback(...)`.

  Those wrappers:

  * Give **Dialyzer** a single place to see the callback’s return type via `@spec`, improving static checks.
  * **Enforce the return shape at runtime**: if the implementation returns a value that doesn’t match the
    behaviour’s callback return type, the wrapper raises `Ash.Error.Framework.InvalidReturnType` with a
    message that identifies the behaviour, callback (e.g. `module.function/arity`), and the allowed
    return shapes.

  Use `call_and_validate_return/5` (or the `check_type!/3` macro where appropriate) inside the
  behaviour’s wrapper to perform the validation.

  ## Dialyzer

  Wrapper `@spec`s were added so that Dialyzer can check return types at call sites. Any remaining
  Dialyzer warnings in the codebase (e.g. in `lib/ash/actions/action.ex`, `lib/ash/can.ex`,
  `lib/ash/actions/read/read.ex`, `lib/ash/actions/read/relationships.ex`, `lib/ash/actions/update/update.ex`,
  `lib/ash/actions/create/create.ex`, `lib/ash/policy/chart/mermaid.ex`) are pre-existing and not
  caused by the behaviour wrapper specs.
  """

  @doc """
  Calls a callback and validates its return value against allowed patterns.

  Takes the same pattern format as `check_type!/3`: atoms (exact match),
  tuples (use `:_` for "any" in a position), or structs (match by struct type).
  Returns the result if it matches any pattern; otherwise raises
  `Ash.Error.Framework.InvalidReturnType` with a message including the
  behaviour name, callback name, and allowed shapes.

  ## Options

  * `:behaviour` - Module name of the behaviour (for error message).
  * `:callback_name` or `:function` - Human-readable callback name, e.g. `"change/3"` (for error message).

  ## Examples

      # With valid return
      call_and_validate_return(MyMod, :change, [cs, opts, ctx], [%Ash.Changeset{}], behaviour: Ash.Resource.Change, callback_name: "change/3")
      # => %Ash.Changeset{...}

      # With invalid return (raises)
      call_and_validate_return(MyMod, :run, [], [:ok], [])
      # => raises Ash.Error.Framework.InvalidReturnType
  """
  @spec call_and_validate_return(module(), atom(), [term()], [term()], keyword()) :: term()
  def call_and_validate_return(module, callback_atom, args_list, allowed_patterns, opts \\ []) do
    result = apply(module, callback_atom, args_list)

    if Enum.any?(allowed_patterns, &matches_pattern?(result, &1)) do
      result
    else
      behaviour_name = opts[:behaviour] && inspect(opts[:behaviour])
      callback_name = resolve_callback_name(opts, callback_atom, args_list)
      pattern_list = Enum.map_join(allowed_patterns, "\n  ", &pattern_to_string/1)

      message =
        [
          "Invalid value returned from #{inspect(module)}.#{callback_name}.",
          if(behaviour_name,
            do:
              "The callback #{behaviour_name}.#{callback_name} expects one of the following return types:",
            else: "Expected one of the following return types:"
          ),
          "",
          "  " <> pattern_list
        ]
        |> Enum.join("\n")

      raise Ash.Error.Framework.InvalidReturnType, message: message
    end
  end

  defp resolve_callback_name(opts, callback_atom, args_list) do
    arity = length(args_list)

    case {opts[:callback_name], opts[:function]} do
      {name, _} when is_binary(name) -> name
      {_, fun} when is_atom(fun) -> "#{fun}/#{arity}"
      _ -> "#{callback_atom}/#{arity}"
    end
  end

  defp matches_pattern?(value, pattern) when is_atom(pattern) do
    value === pattern
  end

  defp matches_pattern?(value, pattern) when is_tuple(pattern) do
    is_tuple(value) and tuple_size(value) == tuple_size(pattern) and
      pattern
      |> Tuple.to_list()
      |> Enum.with_index()
      |> Enum.all?(fn {elem_pattern, i} ->
        elem_value = elem(value, i)
        elem_pattern == :_ or matches_pattern?(elem_value, elem_pattern)
      end)
  end

  defp matches_pattern?(value, pattern) when is_struct(pattern) do
    is_struct(value) and value.__struct__ == pattern.__struct__
  end

  defp matches_pattern?(value, pattern) do
    value == pattern
  end

  defp pattern_to_string(atom) when is_atom(atom) do
    inspect(atom)
  end

  defp pattern_to_string(tuple) when is_tuple(tuple) do
    parts =
      tuple
      |> Tuple.to_list()
      |> Enum.map(fn
        :_ -> "_"
        other -> inspect(other)
      end)

    "{" <> Enum.join(parts, ", ") <> "}"
  end

  defp pattern_to_string(struct) when is_struct(struct) do
    "%#{inspect(struct.__struct__)}{}"
  end

  defp pattern_to_string(other) do
    inspect(other)
  end

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
