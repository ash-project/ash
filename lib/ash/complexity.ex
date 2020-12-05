defmodule Ash.Complexity do
  @moduledoc """
  Ensures that the complexity configuration is enforced for the given query
  """

  alias Ash.Error.SideLoad.ViolatedComplexity

  def validate_side_load(%{side_load: []}, _action), do: :ok

  def validate_side_load(%{side_load: side_loads, resource: resource}) do
    case Ash.Resource.complexity_max(resource) do
      nil ->
        []

      max ->
        side_loads
        |> List.wrap()
        |> Enum.reduce([], fn {relationship, _further}, errors ->
          %{name: name, expected_cardinality: cardinality} =
            Ash.Resource.relationship(resource, relationship)

          case cardinality != nil && cardinality > max do
            false ->
              errors

            true ->
              [
                ViolatedComplexity.exception(
                  resource: resource,
                  relationship: name
                )
                | errors
              ]
          end
        end)
    end
    |> case do
      [] -> :ok
      [error] -> {:error, error}
      errors -> {:errors, errors}
    end
  end
end
