defmodule Ash.Resource.Transformers.ValidationsAndChangesForType do
  @moduledoc """
  Persists global changes/validations and what type they go on.
  """
  use Spark.Dsl.Transformer

  alias Spark.Dsl.Transformer

  def after?(_), do: true

  def transform(dsl_state) do
    validations_by_on =
      dsl_state
      |> Ash.Resource.Info.validations()
      |> Enum.reduce(%{}, fn validation, acc ->
        validation.on
        |> List.wrap()
        |> Enum.reduce(acc, fn on, acc ->
          Map.update(acc, on, [validation], &[validation | &1])
        end)
      end)
      |> Map.new(fn {key, value} -> {key, Enum.reverse(value)} end)

    changes_by_on =
      dsl_state
      |> Ash.Resource.Info.changes()
      |> Enum.reduce(%{}, fn change, acc ->
        change.on
        |> List.wrap()
        |> Enum.reduce(acc, fn on, acc ->
          Map.update(acc, on, [change], &[change | &1])
        end)
      end)
      |> Map.new(fn {key, value} -> {key, Enum.reverse(value)} end)

    {:ok,
     dsl_state
     |> Transformer.persist(:validations_by_on, validations_by_on)
     |> Transformer.persist(:changes_by_on, changes_by_on)}
  end
end
