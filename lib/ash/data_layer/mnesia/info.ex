defmodule Ash.DataLayer.Mnesia.Info do
  @moduledoc "Introspection helpers for Ash.DataLayer.Mnesia"

  alias Spark.Dsl.Extension

  @doc "The mnesia table for a resource"
  def table(resource) do
    Extension.get_opt(resource, [:ets], :private?, resource, true)
  end
end
