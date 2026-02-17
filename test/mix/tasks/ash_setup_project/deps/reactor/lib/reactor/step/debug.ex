# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Step.Debug do
  @moduledoc """
  A very simple step which sends debug information to `Logger`.

  ## Options

  * `level` - the log level to use.
  """

  use Reactor.Step
  require Logger

  @type options :: [level_option]
  @type level_option :: {:level, Logger.level()}

  @doc false
  @impl true
  @spec run(Reactor.inputs(), Reactor.context(), options) :: {:ok | :error, any}
  def run(arguments, context, options) do
    {level, options} = Keyword.pop(options, :level, :debug)

    message = """
    # Debug information for step `#{inspect(context.current_step.name)}`.

    ## Arguments

    ```
    #{inspect(arguments)}
    ```

    ## Context
    ```
    #{inspect(context)}
    ```

    ## Options
    ```
    #{inspect(options)}
    ```
    """

    Logger.log(level, message)

    {:ok, nil}
  end
end
