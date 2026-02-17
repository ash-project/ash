# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Step.TransformAll do
  @moduledoc """
  A built-in step which applies a transformation function to all it's arguments.

  The returned map is used as the arguments to the step, instead of the step's
  defined arguments.


  > #### Tip {: .tip}
  >
  > This step will be emitted by the builder when a step wants to transform all
  > it's arguments.
  >
  > Most likely you will never need to use this step directly.
  """

  use Reactor.Step
  alias Reactor.{Error.Invalid.TransformError, Step.Transform}

  @doc false
  @impl true
  @spec run(Reactor.inputs(), Reactor.context(), keyword) :: {:ok | :error, any}
  def run(arguments, context, options) do
    case Transform.run(%{value: arguments}, context, options) do
      {:ok, result} when is_map(result) ->
        {:ok, result}

      {:ok, result} ->
        {:error,
         TransformError.exception(
           input: arguments,
           output: result,
           error: "Step transformers must return a map to use as replacement arguments."
         )}

      {:error, reason} ->
        {:error, TransformError.exception(input: arguments, error: reason)}
    end
  end
end
