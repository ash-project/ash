defmodule Ash.Flow.Transformers.ValidateUniqueNames do
  @moduledoc "Validates that steps have unique names."
  use Ash.Dsl.Transformer

  def before?(_), do: true

  def transform(flow, dsl_state) do
    flow
    |> Ash.Flow.Info.steps()
    |> unnest()
    |> Enum.map(& &1.name)
    |> Enum.group_by(& &1)
    |> Enum.find_value({:ok, dsl_state}, fn
      {_, [_]} ->
        nil

      {name, [_ | _] = dupes} ->
        {:error,
         Ash.Error.Dsl.DslError.exception(
           module: flow,
           path: [:flow, :steps],
           message:
             "Step names must be unique, but #{Enum.count(dupes)} steps share the name #{name}."
         )}
    end)
  end

  defp unnest(steps) do
    Enum.flat_map(steps, fn
      %{steps: steps} = step ->
        [step | unnest(steps)]

      step ->
        [step]
    end)
  end
end
