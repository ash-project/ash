defmodule Ash.DataLayer.Transformers.RequirePreCheckWith do
  @moduledoc """
  Ensures that all identities have a `pre_check_with` configured, or raises.
  """
  use Spark.Dsl.Transformer

  require Logger

  @impl true
  def after_compile?, do: true

  @impl true
  def transform(resource, dsl) do
    resource
    |> Ash.Resource.Info.identities()
    |> Enum.filter(fn identity ->
      is_nil(identity.pre_check_with)
    end)
    |> case do
      [] ->
        {:ok, dsl}

      identities ->
        if function_exported?(resource, :testing_identities, 0) do
          {:ok, dsl}
        else
          {:error,
           Spark.Error.DslError.exception(
             module: resource,
             message: """
             The data layer for #{inspect(resource)} does not support native checking of identities.

             Identities: #{Enum.map_join(identities, ", ", & &1.name)}

             They must specify the `pre_check_with` option.
             """
           )}
        end
    end
  end
end
