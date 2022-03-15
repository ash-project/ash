defmodule Ash.Flow.Transformers.SetApi do
  @moduledoc "Sets the api on the steps of a flow to the default api, unless an api is set explicitly."
  use Ash.Dsl.Transformer

  def before?(_), do: true

  def transform(flow, dsl_state) do
    api = Ash.Flow.Info.api(flow)

    flow
    |> Ash.Flow.Info.steps()
    |> Enum.filter(&Map.has_key?(&1, :api))
    |> Enum.reduce({:ok, dsl_state}, fn step, {:ok, dsl_state} ->
      if step.api do
        {:ok, dsl_state}
      else
        if api do
          {:ok,
           Ash.Dsl.Transformer.replace_entity(
             dsl_state,
             [:steps],
             %{step | api: api},
             &(&1.name == step.name)
           )}
        else
          {:error,
           Ash.Error.Dsl.DslError.exception(
             module: flow,
             path: [:flow, :steps, step.name, :api],
             message:
               "Api is required for #{step.__struct__} steps. A default one can be provided in the `flow` section."
           )}
        end
      end
    end)
  end
end
