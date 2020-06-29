defmodule Ash.Api.Transformers.EnsureResourcesCompiled do
  @moduledoc """
  Ensures that all resources for a given api are compiled.

  This is required for later transformers.
  """
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
