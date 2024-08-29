defmodule Ash.Resource.Transformers.CacheCalculations do
  @moduledoc """
  Persists commonly used calculation information.
  """
  use Spark.Dsl.Transformer

  alias Spark.Dsl.Transformer

  def after?(_), do: true

  def transform(dsl_state) do
    calculations =
      Ash.Resource.Info.calculations(dsl_state)

    calculations_by_name =
      calculations
      |> Enum.reduce(%{}, fn %{name: name} = calc, acc ->
        acc
        |> Map.put(name, calc)
        |> Map.put(to_string(name), calc)
      end)

    calculation_names = Enum.map(calculations, & &1.name)

    {:ok,
     persist(
       dsl_state,
       %{
         calculation_names: calculation_names,
         calculations_by_name: calculations_by_name
       }
     )}
  end

  defp persist(dsl, map) do
    Enum.reduce(map, dsl, fn {key, value}, dsl ->
      Transformer.persist(dsl, key, value)
    end)
  end
end
