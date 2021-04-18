defmodule Ash.Api.Transformers.EnsureResourcesCompiled do
  @moduledoc """
  Ensures that all resources for a given api are compiled.

  This is required for later transformers.
  """
  use Ash.Dsl.Transformer

  alias Ash.Dsl.Transformer
  require Logger

  def transform(_module, dsl) do
    dsl
    |> Transformer.get_entities([:resources])
    |> Enum.filter(& &1.warn_on_compile_failure?)
    |> Enum.map(& &1.resource)
    |> Enum.reject(fn resource ->
      case Code.ensure_compiled(resource) do
        {:module, _module} ->
          true

        {:error, error} ->
          Logger.error(
            "Could not ensure that #{inspect(resource)} was compiled: #{inspect(error)}"
          )

          # The module is being compiled but is in a deadlock that may or may not be resolved
          false
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
