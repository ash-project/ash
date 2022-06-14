defmodule Ash.Registry.ResourceValidations.Transformers.ValidateRelatedResourceInclusion do
  @moduledoc """
  Ensures that all related resources are included in an API.
  """
  use Ash.Dsl.Transformer

  @impl true
  def after_compile?, do: true

  @impl true
  def after?(Ash.Registry.ResourceValidations.Transformers.EnsureResourcesCompiled), do: true
  def after?(_), do: false

  @impl true
  def transform(module, dsl) do
    resources = Ash.Registry.entries(module)

    resources
    |> Enum.flat_map(&get_all_related_resources(&1, resources))
    |> Enum.uniq()
    |> Enum.reject(&(&1 in resources))
    |> case do
      [] ->
        {:ok, dsl}

      resources ->
        raise "Resources #{Enum.map_join(resources, ", ", &inspect/1)} must be included in #{inspect(module)}"
    end
  end

  defp get_all_related_resources(resource, checked) do
    resource
    |> Ash.Resource.Info.relationships()
    |> Enum.reject(& &1.api)
    |> Enum.flat_map(fn
      %{type: :many_to_many} = relationship ->
        [relationship.through, relationship.destination]

      relationship ->
        [relationship.destination]
    end)
    |> Enum.reject(&(&1 in checked))
    |> Enum.flat_map(fn resource ->
      [resource | get_all_related_resources(resource, [resource | checked])]
    end)
  end
end
