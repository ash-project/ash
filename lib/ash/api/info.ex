defmodule Ash.Api.Info do
  @moduledoc "Introspection tools for Ash.Api"

  alias Ash.Error.Invalid.NoSuchResource

  alias Spark.Dsl.Extension

  @doc """
  Gets the resources of an Api module. DO NOT USE AT COMPILE TIME.

  If you need the resource list at compile time, you will need to introduce a compile time
  dependency on all of the resources, and therefore should use the registry directly. `Registry |> Ash.Registry.Info.entries()`.
  """
  @spec resources(Ash.Api.t()) :: list(Ash.Resource.t())
  def resources(api) do
    if registry = registry(api) do
      Ash.Registry.Info.entries(registry)
    else
      []
    end
  end

  @doc "The resource registry for an api"
  @spec registry(Ash.Api.t()) :: atom | nil
  def registry(api) do
    Extension.get_opt(api, [:resources], :registry, nil, true)
  end

  @doc "The allow MFA for an api"
  @spec allow(Ash.Api.t()) :: mfa() | nil
  def allow(api) do
    Extension.get_opt(api, [:resources], :allow, nil, true)
  end

  @doc "The execution timeout for an api"
  @spec timeout(Ash.Api.t()) :: nil | :infinity | integer()
  def timeout(api) do
    Extension.get_opt(api, [:execution], :timeout, 30_000, true)
  end

  @doc "Whether or not the actor is always required for an api"
  @spec require_actor?(Ash.Api.t()) :: boolean
  def require_actor?(api) do
    Extension.get_opt(api, [:authorization], :require_actor?, false, true)
  end

  @doc "When authorization should happen for a given api"
  @spec authorize(Ash.Api.t()) :: :when_requested | :always | :by_default
  def authorize(api) do
    Extension.get_opt(api, [:authorization], :authorize, :when_requested, true)
  end

  @doc "Whether or not the api allows unregistered resources to be used with it"
  @spec allow_unregistered?(Ash.Api.t()) :: atom | nil
  def allow_unregistered?(api) do
    Extension.get_opt(api, [:resources], :allow_unregistered?, nil)
  end

  @doc """
  Returns `{:ok, resource}` if the resource can be used by the api, or `{:error, error}`.
  """
  @spec resource(Ash.Api.t(), Ash.Resource.t()) ::
          {:ok, Ash.Resource.t()} | {:error, Ash.Error.t()}
  def resource(api, resource) do
    cond do
      allow_unregistered?(api) ->
        if Spark.Dsl.is?(resource, Ash.Resource) do
          resource
        else
          nil
        end

      Ash.Resource.Info.embedded?(resource) ->
        resource

      true ->
        api
        |> resources()
        |> Enum.find(&(&1 == resource))
    end
    |> case do
      nil ->
        if allowed?(allow(api), resource) do
          {:ok, resource}
        else
          {:error, NoSuchResource.exception(resource: resource)}
        end

      resource ->
        {:ok, resource}
    end
  end

  @spec allowed?(mfa | nil, module()) :: boolean
  defp allowed?({m, f, a}, resource) do
    apply(m, f, List.wrap(a) ++ [resource])
  end

  defp allowed?(_, _), do: false
end
