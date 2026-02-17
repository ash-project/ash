# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Step.Transform do
  @moduledoc """
  The built-in step for executing input and argument transformations.

  Expects a single argument named `value` which contains the value to be
  transformed.

  ## Options

  * `fun` - a one or two arity function or MFA to use to modify the `value`
    argument.

  > #### Tip {: .tip}
  >
  > This step is emitted by the builder when an argument needs to be transformed
  > before being passed into a step.
  >
  > Most likely you will never need to use this step directly.
  """

  alias Reactor.{Error.Invalid.MissingArgumentError, Error.Invalid.TransformError, Step}
  use Step
  @behaviour Reactor.Mermaid

  @doc false
  @impl true
  @spec run(Reactor.inputs(), Reactor.context(), keyword) :: {:ok | :error, any}
  def run(arguments, context, options) do
    case Map.fetch(arguments, :value) do
      {:ok, value} ->
        do_transform(value, options)

      :error ->
        {:error,
         MissingArgumentError.exception(
           step: context.current_step,
           argument: :value,
           arguments: arguments
         )}
    end
  end

  @doc false
  @impl true
  def to_mermaid(%{impl: {__MODULE__, opts}} = step, options) do
    __MODULE__.Mermaid.to_mermaid(step, opts[:fun], options)
  end

  defp do_transform(value, opts) do
    case Keyword.pop(opts, :fun) do
      {fun, _opts} when is_function(fun, 1) ->
        {:ok, fun.(value)}

      {fun, opts} when is_function(fun, 2) ->
        {:ok, fun.(value, opts)}

      {{m, f, a}, _opts} when is_atom(m) and is_atom(f) and is_list(a) ->
        {:ok, apply(m, f, [value | a])}
    end
  rescue
    error -> {:error, TransformError.exception(input: value, error: error)}
  end
end
