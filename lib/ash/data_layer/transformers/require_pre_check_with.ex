defmodule Ash.DataLayer.Transformers.RequirePreCheckWith do
  @moduledoc """
  Ensures that all identities have a `pre_check_with` configured, or raises.
  """
  use Spark.Dsl.Transformer

  alias Spark.Dsl.Transformer

  require Logger

  @impl true
  def after_compile?, do: true

  @impl true
  def transform(dsl) do
    resource = Transformer.get_persisted(dsl, :module)

    dsl
    |> Transformer.get_entities([:identities])
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
