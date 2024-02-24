defmodule Ash.Domain.Verifiers.EnsureNoEmbeds do
  @moduledoc """
  Ensures that all resources for a given domain are not embeds.
  """
  use Spark.Dsl.Verifier
  alias Spark.Dsl.Verifier

  @impl true
  def verify(dsl) do
    dsl
    |> Verifier.get_entities([:resources])
    |> Enum.map(& &1.resource)
    |> Enum.filter(&Ash.Resource.Info.embedded?/1)
    |> case do
      [] ->
        :ok

      rejected ->
        {:error,
         "Embedded resources should not be listed in the domain. Please remove #{inspect(rejected)}."}
    end
  end
end
