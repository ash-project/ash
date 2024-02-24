defmodule Ash.Domain.Verifiers.EnsureResourcesCompiled do
  @moduledoc """
  Ensures that all resources for a given domain are compiled.
  """
  use Spark.Dsl.Verifier
  alias Spark.Dsl.Verifier

  require Logger

  @spec verify(map) :: :ok
  def verify(dsl) do
    dsl
    |> Verifier.get_entities([:resources])
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
        :ok

      rejected ->
        for {resource, error} <- rejected do
          Logger.error(
            "Could not ensure that #{inspect(resource)} was compiled: #{inspect(error)}"
          )
        end

        :ok
    end
  end
end
