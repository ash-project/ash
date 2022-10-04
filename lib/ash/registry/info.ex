defmodule Ash.Registry.Info do
  @moduledoc "Introspection helpers for `Ash.Registry`"

  alias Spark.Dsl.Extension

  @doc "Whether or not the registry will warn if it has no entries"
  @spec warn_on_empty?(Ash.Registry.t()) :: boolean
  def warn_on_empty?(registry) do
    Extension.get_opt(registry, [:entries], :warn_on_empty?, true, true)
  end

  @doc "The list of entries in the registry"
  @spec entries(Ash.Registry.t()) :: list(module)
  def entries(registry) do
    case registry |> Extension.get_entities([:entries]) |> Enum.map(& &1.entry) do
      [] ->
        registry |> Extension.get_entities([:resources]) |> Enum.map(& &1.resource)

      other ->
        other
    end
  end
end
