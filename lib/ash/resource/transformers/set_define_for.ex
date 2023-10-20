defmodule Ash.Resource.Transformers.SetDefineFor do
  @moduledoc false
  use Spark.Dsl.Transformer

  def transform(dsl) do
    api = Spark.Dsl.Transformer.get_persisted(dsl, :api)

    if api do
      dsl =
        Spark.Dsl.Transformer.persist(dsl, :api, api)

      if Ash.Resource.Info.define_interface_for(dsl) do
        {:ok, dsl}
      else
        {:ok, Spark.Dsl.Transformer.set_option(dsl, [:code_interface], :define_for, api)}
      end
    else
      {:ok, dsl}
    end
  end
end
