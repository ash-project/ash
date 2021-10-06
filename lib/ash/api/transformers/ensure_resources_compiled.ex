defmodule Ash.Api.Transformers.EnsureResourcesCompiled do
  @moduledoc """
  Ensures that all resources for a given api are compiled.

  This is required for later transformers.
  """
  use Ash.Dsl.Transformer

  alias Ash.Dsl.Transformer
  require Logger

  @impl true
  def after_compile?, do: true

  @impl true
  def transform(_api, dsl) do
    dsl
    |> Transformer.get_entities([:resources])
    |> Enum.map(& &1.resource)
    |> Enum.each(fn resource ->
      resource.ash_dsl_config()
    end)
  end
end
