defmodule Ash.Resource.Transformers.PersistSpecs do
  @moduledoc """
  This transformer is no longer needed since specs are persisted directly in the Resource module.
  Keeping this placeholder for now in case we need additional spec processing later.
  """

  use Spark.Dsl.Transformer

  @impl true
  def transform(dsl_state) do
    # No-op transformer for now
    {:ok, dsl_state}
  end
end
