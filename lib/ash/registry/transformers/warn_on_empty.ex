defmodule Ash.Registry.Transformers.WarnOnEmpty do
  @moduledoc "Warns if a registry has no entries in it"
  use Spark.Dsl.Transformer

  def transform(registry, dsl) do
    if Ash.Registry.Info.warn_on_empty?(registry) do
      case Ash.Registry.Info.entries(registry) do
        [] ->
          {:warn, dsl, "#{inspect(registry)} has no entries."}

        _ ->
          {:ok, dsl}
      end
    else
      {:ok, dsl}
    end
  end
end
