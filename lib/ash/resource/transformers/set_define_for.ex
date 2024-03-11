defmodule Ash.Resource.Transformers.SetDefineFor do
  @moduledoc false
  use Spark.Dsl.Transformer

  def transform(dsl) do
    api = Spark.Dsl.Transformer.get_persisted(dsl, :api)

    if api && !Ash.Resource.Info.define_interface_for(dsl) do
      {:ok, Spark.Dsl.Transformer.set_option(dsl, [:code_interface], :define_for, api)}
    else
      {:ok, dsl}
    end
  end
end
