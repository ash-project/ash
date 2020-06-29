defmodule Ash.Api.Transformers.EnsureResourcesCompiled do
  use Ash.Dsl.Transformer

  alias Ash.Dsl.Transformer

  @extension Ash.Api.Dsl

  def transform(_module, dsl) do
    dsl
    |> Transformer.get_entities([:resources], @extension)
    |> Enum.map(& &1.resource)
    |> Enum.each(&Code.ensure_compiled/1)

    {:ok, dsl}
  end
end
