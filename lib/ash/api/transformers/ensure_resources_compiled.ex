defmodule Ash.Api.Transformers.EnsureResourcesCompiled do
  @moduledoc """
  Ensures that all resources for a given api are compiled.

  This is required for later transformers.
  """
  use Ash.Dsl.Transformer

  alias Ash.Dsl.Transformer
  require Logger

  def transform(module, dsl, times \\ 3) do
    dsl
    |> Transformer.get_entities([:resources])
    |> Enum.filter(& &1.warn_on_compile_failure?)
    |> Enum.map(& &1.resource)
    |> Enum.map(fn resource ->
      try do
        # This is to get the compiler to ensure that the resource is compiled
        # For some very strange reason, `Code.ensure_compiled/1` isn't enough
        resource.ash_dsl_config()
      rescue
        _ ->
          :ok
      end

      case Code.ensure_compiled(resource) do
        {:module, _module} ->
          false

        {:error, error} ->
          # The module is being compiled but is in a deadlock that may or may not be resolved
          {resource, error}
      end
    end)
    |> Enum.filter(& &1)
    |> case do
      [] ->
        {:ok, dsl}

      rejected ->
        if times == 0 do
          for {resource, error} <- rejected do
            Logger.error(
              "Could not ensure that #{inspect(resource)} was compiled: #{inspect(error)}"
            )
          end

          :halt
        else
          transform(module, dsl, times - 1)
        end
    end
  end
end
