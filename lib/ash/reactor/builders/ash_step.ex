defimpl Reactor.Dsl.Build, for: Ash.Reactor.Dsl.AshStep do
  @moduledoc false

  alias Ash.Reactor.AshStep
  alias Reactor.Builder
  alias Spark.Error.DslError
  import Ash.Reactor.BuilderUtils

  @doc false
  @impl true
  def build(ash_step, reactor) do
    with {:ok, reactor} <- ensure_hooked(reactor) do
      Builder.add_step(
        reactor,
        ash_step.name,
        {AshStep, [run: ash_step.run, compensate: ash_step.compensate, undo: ash_step.undo]},
        ash_step.arguments,
        async?: ash_step.async?,
        max_retries: ash_step.max_retries,
        transform: ash_step.transform,
        ref: :step_name
      )
    end
  end

  @doc false
  @impl true
  def transform(_update, dsl_state), do: {:ok, dsl_state}

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

  defp rewrite_step(step, _dsl_state)
       when is_nil(step.run) and is_nil(step.compensate) and is_nil(step.undo) and
              not is_nil(step.impl),
       do: {:ok, step}

  defp rewrite_step(step, _dsl_state),
    do:
      {:ok,
       %{
         step
         | impl:
             {Ash.Reactor.AshStep, run: step.run, compensate: step.compensate, undo: step.undo},
           run: nil,
           compensate: nil,
           undo: nil
       }}
end
