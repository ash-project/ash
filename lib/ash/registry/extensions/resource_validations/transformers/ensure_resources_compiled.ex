defmodule Ash.Registry.ResourceValidations.Transformers.EnsureResourcesCompiled do
  @moduledoc """
  Ensures that all resources for a given registry are compiled.
  """
  use Spark.Dsl.Transformer
  alias Spark.Dsl.Transformer

  require Logger

  @impl true
  def after_compile?, do: true

  @impl true
  def transform(dsl) do
    dsl
    |> Transformer.get_entities([:entries])
    |> Enum.map(& &1.entry)
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
        for {resource, error} <- rejected do
          Logger.error(
            "Could not ensure that #{inspect(resource)} was compiled: #{inspect(error)}"
          )
        end

        {:ok, dsl}
    end
  end
end
