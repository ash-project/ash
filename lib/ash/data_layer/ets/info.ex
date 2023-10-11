defmodule Ash.DataLayer.Ets.Info do
  @moduledoc "Introspection helpers for the Ets data layer"

  alias Spark.Dsl.Extension

  @doc "Whether or not the ets table for the resource should be private"
  @spec private?(Ash.Resource.t() | Spark.Dsl.t()) :: boolean
  def private?(resource) do
    Extension.get_opt(resource, [:ets], :private?, false, true)
  end

  @doc "The ets table name for a resource"
  @spec table(Ash.Resource.t() | Spark.Dsl.t()) :: boolean
  def table(resource) do
    Extension.get_opt(resource, [:ets], :table, resource, true) || resource
  end
end
