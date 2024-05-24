defmodule Ash.Resource.Transformers.CacheUniqueKeys do
  @moduledoc "Stores the set of unique keys for a resource"
  use Spark.Dsl.Transformer

  alias Spark.Dsl.Transformer

  def transform(dsl_state) do
    unique_keys =
      dsl_state
      |> Ash.Resource.Info.identities()
      |> Enum.map(&%{keys: &1.keys, type: :identity, nils_distinct?: &1.nils_distinct?})

    pkey = Ash.Resource.Info.primary_key(dsl_state)

    unique_keys =
      if !Enum.empty?(pkey) &&
           Enum.all?(pkey, fn pkey ->
             Ash.Resource.Info.attribute(dsl_state, pkey).public?
           end) do
        [%{keys: pkey, type: :primary_key, nils_distinct?: false} | unique_keys]
      else
        unique_keys
      end

    {:ok, Transformer.persist(dsl_state, :unique_keys, unique_keys)}
  end
end
