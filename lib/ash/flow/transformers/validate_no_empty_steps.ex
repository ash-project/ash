defmodule Ash.Flow.Transformers.ValidateNoEmptySteps do
  @moduledoc "Validates that no nested steps contain no steps"
  use Spark.Dsl.Transformer

  def transform(flow, dsl_state) do
    flow
    |> Ash.Flow.Info.steps()
    |> validate_steps!(flow)

    {:ok, dsl_state}
  end

  defp validate_steps!(steps, flow, trail \\ []) do
    for step <- steps do
      if Map.has_key?(step, :steps) do
        if Enum.empty?(step.steps || []) do
          raise Spark.Error.DslError,
            module: flow,
            message: "Must have at least one step.",
            path: Enum.reverse(trail, [step.name])
        else
          validate_steps!(step.steps, flow, [step.name | trail])
        end
      end
    end
  end
end
