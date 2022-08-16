defmodule Ash.Registry.Transformers.WarnOnEmpty do
  @moduledoc "Warns if a registry has no entries in it"
  use Spark.Dsl.Transformer
  alias Spark.Dsl.Transformer

  def transform(dsl) do
    if Transformer.get_option(dsl, [:entries], :warn_on_empty?) do
      case Transformer.get_entities(dsl, [:entries]) do
        [] ->
          {:warn, dsl, "Registry has no entries."}

        _ ->
          {:ok, dsl}
      end
    else
      {:ok, dsl}
    end
  end
end
