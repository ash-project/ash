defmodule Ash.Flow.Transformers.SetDomain do
  @moduledoc "Sets the domain on the steps of a flow to the default domain, unless an domain is set explicitly."
  use Spark.Dsl.Transformer
  alias Spark.Dsl.Transformer

  def before?(_), do: true

  def transform(dsl_state) do
    domain = Transformer.get_option(dsl_state, [:flow], :domain)

    dsl_state
    |> Transformer.get_entities([:steps])
    |> Enum.map(&set_domain(&1, domain))
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

  def set_domain(step, default) do
    if Map.has_key?(step, :domain) do
      step = %{step | domain: step.domain || default}

      if step.domain do
        step
      else
        raise Spark.Error.DslError,
          path: [:flow, :steps, step.name, :domain],
          message:
            "Domain is required for #{step.__struct__} steps. A default one can be provided in the `flow` section."
      end
    else
      step
    end
    |> set_nested_domains(default)
  end

  defp set_nested_domains(%{steps: steps} = step, default) do
    %{step | steps: Enum.map(steps, &set_domain(&1, default))}
  end

  defp set_nested_domains(step, _), do: step
end
