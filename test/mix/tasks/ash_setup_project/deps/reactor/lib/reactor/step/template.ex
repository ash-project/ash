# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Step.Template do
  @opt_schema Spark.Options.new!(
                template: [
                  type: :string,
                  required: true,
                  doc: "The EEx template"
                ],
                trim: [
                  type: :boolean,
                  required: false,
                  default: false,
                  doc:
                    "Whether to trim whitespace before and after the template. See `EEx#options`"
                ]
              )

  @moduledoc """
  A step which renders an `EEx` template using it's arguments as the assigns.

  ## Options

  #{Spark.Options.docs(@opt_schema)}
  """
  use Reactor.Step

  @doc false
  @impl true
  # sobelow_skip ["RCE.EEx"]
  def run(arguments, _, options) do
    with {:ok, options} <- Spark.Options.validate(options, @opt_schema) do
      result = EEx.eval_string(options[:template], [assigns: arguments], trim: options[:trim])
      {:ok, result}
    end
  end
end
