defmodule Ash.Flow.Verifiers.ValidateNoEmptySteps do
  @moduledoc "Validates that no nested steps contain no steps"
  use Spark.Dsl.Verifier
  alias Spark.Dsl.Verifier

  def verify(dsl_state) do
    dsl_state
    |> Verifier.get_entities([:steps])
    |> validate_steps!()

    :ok
  end

  defp validate_steps!(steps, trail \\ []) do
    for step <- steps do
      if Map.has_key?(step, :steps) do
        if Enum.empty?(step.steps || []) do
          raise Spark.Error.DslError,
            message: "Must have at least one step.",
            path: Enum.reverse(trail, [step.name])
        else
          validate_steps!(step.steps, [step.name | trail])
        end
      end
    end
  end
end
