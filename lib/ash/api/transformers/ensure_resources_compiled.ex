defmodule Ash.Api.Transformers.EnsureResourcesCompiled do
  @moduledoc """
  Ensures that all resources for a given api are compiled.

  This is required for later transformers.
  """
  use Ash.Dsl.Transformer

  alias Ash.Dsl.Transformer

  def transform(_module, dsl) do
    dsl
    |> Transformer.get_entities([:resources])
    |> Enum.map(& &1.resource)
    |> Enum.filter(fn resource ->
      case Code.ensure_compiled(resource) do
        {:module, _module} ->
          false

        _ ->
          true
      end
    end)
    |> case do
      [] ->
        {:ok, dsl}

      _ ->
        :halt
    end
  end
end
