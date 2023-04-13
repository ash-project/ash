defmodule Ash.DataLayer.Ets.Verifier do
  use Spark.Dsl.Verifier

  def verify(dsl_state) do
    multitenancy_enabled? = Map.has_key?(dsl_state, [:multitenancy])
    has_explicit_table_name? = Spark.Dsl.Verifier.get_option(dsl_state, [:ets], :table)

    if multitenancy_enabled? && has_explicit_table_name? do
      {:error, Spark.Error.DslError.exception(
        path: [:ets, :table],
        module: Spark.Dsl.Verifier.get_persisted(dsl_state, :module),
        message: """
        The ETS data layer does not support both multitenancy and an explicit table name at the same time.
        """
      )}
    else
      :ok
    end
  end
end
