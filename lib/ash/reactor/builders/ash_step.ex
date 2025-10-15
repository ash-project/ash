# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defimpl Reactor.Dsl.Build, for: Ash.Reactor.Dsl.AshStep do
  @moduledoc false

  alias Reactor.Builder
  alias Spark.Error.DslError

  @doc false
  @impl true
  def build(ash_step, reactor) do
    with {:ok, ash_step} <- rewrite_step(ash_step, reactor.id) do
      Builder.add_step(
        reactor,
        ash_step.name,
        ash_step.impl,
        ash_step.arguments,
        async?: ash_step.async?,
        guards: ash_step.guards,
        max_retries: ash_step.max_retries,
        ref: :step_name,
        transform: ash_step.transform
      )
    end
  end

  @doc false
  @impl true
  def verify(_ash_step, _dsl_state), do: :ok

  defp rewrite_step(step, module) when is_nil(step.impl) and is_nil(step.run),
    do:
      {:error,
       DslError.exception(
         module: module,
         path: [:reactor, :step, step.name],
         message: "Step has no implementation"
       )}

  defp rewrite_step(step, module) when not is_nil(step.impl) and not is_nil(step.run),
    do:
      {:error,
       DslError.exception(
         module: module,
         path: [:reactor, :step, step.name],
         message: "Step has both an implementation module and a run function"
       )}

  defp rewrite_step(step, module)
       when not is_nil(step.impl) and not is_nil(step.compensate),
       do:
         {:error,
          DslError.exception(
            module: module,
            path: [:reactor, :step, step.name],
            message: "Step has both an implementation module and a compensate function"
          )}

  defp rewrite_step(step, module) when not is_nil(step.impl) and not is_nil(step.undo),
    do:
      {:error,
       DslError.exception(
         module: module,
         path: [:reactor, :step, step.name],
         message: "Step has both an implementation module and a undo function"
       )}

  defp rewrite_step(step, _dsl_state),
    do:
      {:ok,
       %{
         step
         | impl:
             {Ash.Reactor.AshStep,
              run: step.run, compensate: step.compensate, undo: step.undo, impl: step.impl},
           run: nil,
           compensate: nil,
           undo: nil
       }}
end
