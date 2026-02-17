# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Error.Invalid.ForcedFailureError do
  @moduledoc """
  This error is returned when the `flunk` DSL entity or the `Reactor.Step.Fail`
  step are called.
  """

  use Reactor.Error,
    fields: [:arguments, :step_name, :message, :context, :options],
    class: :invalid

  @type t :: %__MODULE__{
          __exception__: true,
          arguments: %{atom => any},
          step_name: any,
          message: String.t(),
          context: map,
          options: keyword
        }

  @doc false
  @impl true
  def message(error), do: error.message
end
