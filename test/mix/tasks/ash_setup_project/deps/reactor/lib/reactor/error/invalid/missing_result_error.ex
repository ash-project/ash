# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Error.Invalid.MissingResultError do
  @moduledoc """
  This error is returned when a step attempts to consume an intermediate result
  which is not present in the Reactor state.
  """
  use Reactor.Error, fields: [:argument, :reactor, :step], class: :invalid

  @doc false
  @impl true
  def message(error) do
    inputs =
      error.reactor.inputs
      |> Enum.map_join("\n", &"  * `#{inspect(&1)}`")

    """
    # Missing Result Error

    The step `#{inspect(error.step.name)}` is expecting the Reactor to have an existing result named `#{inspect(error.argument.source.name)}` however it is not present.
    #{did_you_mean?(error.argument.source.name, Map.keys(error.reactor.intermediate_results))}

    ## `step`:

    #{inspect(error.step)}

    ## `argument`:

    #{inspect(error.argument)}

    ## Available inputs:

    #{inputs}
    """
  end
end
