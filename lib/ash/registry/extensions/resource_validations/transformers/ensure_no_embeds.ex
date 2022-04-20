defmodule Ash.Registry.ResourceValidations.Transformers.EnsureNoEmbeds do
  @moduledoc """
  Ensures that all resources for a given registry are not embeds.
  """
  use Ash.Dsl.Transformer

  require Logger

  @impl true
  def after_compile?, do: true

  @impl true
  def transform(registry, dsl) do
    registry
    |> Ash.Registry.entries()
    |> Enum.filter(&Ash.Resource.Info.embedded?/1)
    |> case do
      [] ->
        {:ok, dsl}

      rejected ->
        {:error,
         "Embedded resources should not be listed in the registry. Please remove #{inspect(rejected)} from the registry."}
    end
  end
end
