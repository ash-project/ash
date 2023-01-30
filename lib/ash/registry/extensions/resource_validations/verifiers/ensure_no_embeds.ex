defmodule Ash.Registry.ResourceValidations.Verifiers.EnsureNoEmbeds do
  @moduledoc """
  Ensures that all resources for a given registry are not embeds.
  """
  use Spark.Dsl.Verifier
  alias Spark.Dsl.Verifier

  @impl true
  def verify(dsl) do
    dsl
    |> Verifier.get_entities([:entries])
    |> Enum.map(& &1.entry)
    |> Enum.filter(&Ash.Resource.Info.embedded?/1)
    |> case do
      [] ->
        :ok

      rejected ->
        {:error,
         "Embedded resources should not be listed in the registry. Please remove #{inspect(rejected)} from the registry."}
    end
  end
end
