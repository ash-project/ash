defmodule Ash.Resource.Transformers.SetDefineFor do
  @moduledoc false
  use Spark.Dsl.Transformer

  def transform(dsl) do
    api = Spark.Dsl.Transformer.get_persisted(dsl, :api)

    if api && !Ash.Resource.Info.code_interface_api(dsl) do
      {:ok, Spark.Dsl.Transformer.set_option(dsl, [:code_interface], :api, api)}
    else
      {:ok, dsl}
    end
  end
end
