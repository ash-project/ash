# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Error.Invalid.ArgumentSubpathError do
  @moduledoc """
  This error is returned when an argument cannot have a subpath applied to it.
  """
  use Reactor.Error,
    fields: [:argument, :culprit, :culprit_key, :culprit_path, :message, :step, :value],
    class: :invalid

  @doc false
  @impl true
  def message(error) do
    """
    # Argument Subpath Error

    The step `#{inspect(error.step.name)}` is expecting the value for argument `#{inspect(error.argument.name)}` to be able to be subpathed via `#{inspect(error.argument.subpath)}` however #{error.reason}.

    ## `step`:

    #{inspect(error.step)}

    ## `argument`:

    #{inspect(error.argument)}

    ## `value`:

    #{inspect(error.value)}

    ## `culprit`:

    #{inspect(error.culprit)}

    ## `culprit_path`:

    #{inspect(error.culprit_path)}
    """
  end
end
