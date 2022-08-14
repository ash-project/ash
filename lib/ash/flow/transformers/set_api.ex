defmodule Ash.Flow.Transformers.SetApi do
  @moduledoc "Sets the api on the steps of a flow to the default api, unless an api is set explicitly."
  use Spark.Dsl.Transformer

  def before?(_), do: true

  def transform(flow, dsl_state) do
    api = Ash.Flow.Info.api(flow)

    flow
    |> Ash.Flow.Info.steps()
    |> Enum.map(&set_api(&1, flow, api))
    |> Enum.reduce({:ok, dsl_state}, fn step, {:ok, dsl_state} ->
      {:ok,
       Spark.Dsl.Transformer.replace_entity(
         dsl_state,
         [:steps],
         step,
         &(&1.name == step.name)
       )}
    end)
  end

  def set_api(step, flow, default) do
    if Map.has_key?(step, :api) do
      step = %{step | api: step.api || default}

      if step.api do
        step
      else
        raise Spark.Error.DslError,
          module: flow,
          path: [:flow, :steps, step.name, :api],
          message:
            "Api is required for #{step.__struct__} steps. A default one can be provided in the `flow` section."
      end
    else
      step
    end
    |> set_nested_apis(flow, default)
  end

  defp set_nested_apis(%{steps: steps} = step, flow, default) do
    %{step | steps: Enum.map(steps, &set_api(&1, flow, default))}
  end

  defp set_nested_apis(step, _, _), do: step
end
