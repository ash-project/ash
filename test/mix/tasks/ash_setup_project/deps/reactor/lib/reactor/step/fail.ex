# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Step.Fail do
  @moduledoc """
  A very simple step which immediately returns an error.
  """

  use Reactor.Step
  alias Reactor.Error.Invalid.ForcedFailureError

  @doc false
  @impl true
  @spec run(Reactor.inputs(), Reactor.context(), keyword) :: {:error, ForcedFailureError.t()}
  def run(arguments, context, options) do
    {:error,
     ForcedFailureError.exception(
       arguments: arguments.arguments,
       message: arguments.message,
       context: context,
       options: options,
       step_name: context.current_step.name
     )}
  end

  @doc false
  @impl true
  @spec can?(Reactor.Step.t(), Reactor.Step.capability()) :: boolean
  def can?(_step, _capability), do: false
end
