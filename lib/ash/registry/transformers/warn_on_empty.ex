defmodule Ash.Registry.Transformers.WarnOnEmpty do
  @moduledoc "Warns if a registry has no entries in it"
  use Ash.Dsl.Transformer

  def transform(registry, dsl) do
    case Ash.Registry.entries(registry) do
      [] ->
        {:warn, dsl, "#{inspect(registry)} has no entries."}

      _ ->
        {:ok, dsl}
    end
  end
end
