defmodule Ash.DataLayer.Verifiers.IntegerPrimaryKeyNotUsed do
  @moduledoc """
  Ensures that integer primary key is not used, or raises.
  """
  use Spark.Dsl.Verifier

  alias Spark.Dsl.Verifier

  require Logger

  @impl true
  def verify(dsl) do
    dsl
    |> Verifier.get_entities([:attributes])
    |> Enum.filter(fn attribute ->
      attribute.primary_key? && attribute.type == Ash.Type.Integer
    end)
    |> case do
      [] ->
        :ok

      attributes ->
        {:error,
         Spark.Error.DslError.exception(
           message: """
           The data layer does not support integer primary key.

           Attributes: #{Enum.map_join(attributes, ", ", & &1.name)}

           Consider using uuid_primary_key instead of integer_primary_key
           """
         )}
    end
  end
end
