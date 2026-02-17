# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Step.Switch do
  @moduledoc """
  Conditionally decide which steps should be run at runtime.

  ## Options

  * `matches` - a list of match consisting of predicates and a list of steps to
    execute if the predicate returns a truthy value.  See `t:matches` for more
    information.  Required.
  * `default` - a list of steps to execute if none of the predicates match.
    Optional.
  * `allow_async?` - a boolean indicating whether to allow the steps to be
    executed asynchronously.  Optional.  Defaults to `true`.
  * `on` - the name of the argument to pass into the predicates.  If this
    argument is not provided to this step, then an error will be returned.

  ## Branching behaviour

  Each of the predicates in `matches` are tried in order, until either one
  returns a truthy value, or all the matches are exhausted.

  If there is a match, then the matching steps are emitted into the parent
  running Reactor.

  In the case that no match is found, then the steps provided in the `default`
  option are emitted.  If no default is provided, then an error is returned.

  > #### Tip {: .tip}
  >
  > Execution of predicates stops once the first match is found.  This means
  > that if multiple predicates potentially match, the subsequent ones will
  > never be called.

  ## Returning

  By default the step returns `nil` as it's result.

  You can have the step return the result of a branch by adding a step to the
  branch with the same name as the switch which returns the expected value.
  This will be handled by normal Reactor step emission rules.
  """

  use Reactor.Step
  alias Reactor.Step
  import Reactor.Utils
  @behaviour Reactor.Mermaid

  @typedoc """
  A list of predicates and steps to execute if the predicate returns a truthy
  value.
  """
  @type matches :: [{predicate, [Step.t()]}]

  @typedoc """
  A predicate is a 1-arity function.  It can return anything.  Any result which
  is not `nil` or `false` is considered true.
  """
  @type predicate :: (any -> any)

  @type options :: [match_option | default_option | allow_async_option | on_option]

  @type match_option :: {:matches, matches}
  @type default_option :: {:default, [Step.t()]}
  @type allow_async_option :: {:allow_async?, boolean}
  @type on_option :: {:on, atom}

  @doc false
  @impl true
  @spec run(Reactor.inputs(), Reactor.context(), options) :: {:ok, any} | {:error, any}
  def run(arguments, _context, options) do
    allow_async? = Keyword.get(options, :allow_async?, true)

    with {:ok, on} <- fetch_on(arguments, options),
         {:ok, matches} <- fetch_matches(options),
         :no_match <- find_match(matches, on),
         {:ok, defaults} <- fetch_defaults(options) do
      {:ok, nil, maybe_rewrite_async(defaults, allow_async?)}
    else
      {:match, steps} -> {:ok, nil, maybe_rewrite_async(steps, allow_async?)}
      {:error, reason} -> {:error, reason}
    end
  end

  @doc false
  @impl true
  def to_mermaid(step, options),
    do: __MODULE__.Mermaid.to_mermaid(step, options)

  @doc false
  @impl true
  def nested_steps(options) do
    matches = Keyword.get(options, :matches, [])
    default = Keyword.get(options, :default, [])

    match_steps =
      matches
      |> Enum.flat_map(fn {_predicate, steps} -> steps end)

    match_steps ++ default
  end

  defp find_match(matches, value) do
    Enum.reduce_while(matches, :no_match, fn {predicate, steps}, :no_match ->
      if predicate.(value) do
        {:halt, {:match, steps}}
      else
        {:cont, :no_match}
      end
    end)
  end

  defp fetch_defaults(options) do
    with {:ok, steps} <- Keyword.fetch(options, :default),
         {:ok, steps} <- validate_steps(steps) do
      {:ok, steps}
    else
      {:error, reason} ->
        {:error, reason}

      :error ->
        {:error, "No branch matched in switch and no default branch is set"}
    end
  end

  defp fetch_on(arguments, options) do
    case Keyword.fetch(options, :on) do
      {:ok, on} when is_atom(on) and is_map_key(arguments, on) ->
        {:ok, Map.get(arguments, on)}

      {:ok, _on} ->
        {:error,
         argument_error(:options, "Expected `on` option to match a provided argument", options)}

      :error ->
        {:error, argument_error(:options, "Missing `on` option.")}
    end
  end

  defp fetch_matches(options) do
    case Keyword.fetch(options, :matches) do
      {:ok, matches} -> map_while_ok(matches, &validate_match/1, true)
      :error -> {:error, argument_error(:options, "Missing `matches` option.")}
    end
  end

  defp validate_match({predicate, steps}) do
    with {:ok, predicate} <- capture(predicate),
         {:ok, steps} <- validate_steps(steps) do
      {:ok, {predicate, steps}}
    end
  end

  defp validate_steps(steps) do
    if Enum.all?(steps, &is_struct(&1, Step)),
      do: {:ok, steps},
      else: {:error, argument_error(:steps, "Expected all steps to be a `Reactor.Step` struct.")}
  end

  defp capture(predicate) when is_function(predicate, 1), do: {:ok, predicate}

  defp capture({m, f, []}) when is_atom(m) and is_atom(f),
    do: ensure_exported(m, f, 1, fn -> {:ok, Function.capture(m, f, 1)} end)

  defp capture({m, f, a}) when is_atom(m) and is_atom(f) and is_list(a),
    do:
      ensure_exported(m, f, length(a) + 1, fn ->
        {:ok, fn input -> apply(m, f, [input | a]) end}
      end)

  defp capture(predicate),
    do:
      {:error,
       argument_error(:predicate, "Expected `predicate` to be a 1 arity function", predicate)}

  defp ensure_exported(m, f, arity, callback) do
    if Code.ensure_loaded?(m) && function_exported?(m, f, arity) do
      callback.()
    else
      {:error, "Expected `#{inspect(m)}.#{f}/#{arity}` to be exported."}
    end
  end

  defp maybe_rewrite_async(steps, true), do: steps
  defp maybe_rewrite_async(steps, false), do: Enum.map(steps, &%{&1 | async?: false})
end
