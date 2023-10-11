defmodule Ash.DataLayer.Verifiers.RequirePreCheckWith do
  @moduledoc """
  Ensures that all identities have a `pre_check_with` configured, or raises.
  """
  use Spark.Dsl.Verifier

  alias Spark.Dsl.Verifier

  require Logger

  @impl true
  def verify(dsl) do
    resource = Verifier.get_persisted(dsl, :module)

    dsl
    |> Verifier.get_entities([:identities])
    |> Enum.filter(fn identity ->
      is_nil(identity.pre_check_with)
    end)
    |> case do
      [] ->
        :ok

      identities ->
        if function_exported?(resource, :testing_identities, 0) do
          :ok
        else
          {:error,
           Spark.Error.DslError.exception(
             message: """
             The data layer does not support native checking of identities.

             Identities: #{Enum.map_join(identities, ", ", & &1.name)}

             Must specify the `pre_check_with` option.
             """
           )}
        end
    end
  end
end
