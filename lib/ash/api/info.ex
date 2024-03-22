defmodule Ash.Api.Info do
  @moduledoc "Introspection tools for Ash.Api"

  alias Ash.Error.Invalid.{NoSuchResource, ResourceNotAllowed}

  alias Spark.Dsl.Extension

  @doc """
  Gets the resources of an Api module. DO NOT USE AT COMPILE TIME.

  If you need the resource list at compile time, use `depend_on_resources/1`
  """
  @spec resources(Ash.Api.t()) :: list(Ash.Resource.t())
  def resources(api) do
    registry_resources =
      if registry = registry(api) do
        Ash.Registry.Info.entries(registry)
      else
        []
      end

    api_resources =
      api
      |> Extension.get_entities([:resources])
      |> Enum.map(& &1.resource)

    registry_resources ++ api_resources
  end

  def find_manage_relationships_with_identity_not_configured(otp_app) do
    otp_app
    |> Application.get_env(:ash_apis, [])
    |> Enum.flat_map(&Ash.Api.Info.resources/1)
    |> Enum.flat_map(fn resource ->
      resource
      |> Ash.Resource.Info.actions()
      |> Enum.flat_map(fn action ->
        action
        |> Map.get(:changes, [])
        |> Enum.flat_map(fn
          %{change: {Ash.Resource.Change.ManageRelationship, opts}} ->
            related = Ash.Resource.Info.related(resource, opts[:relationship])
            identities = Ash.Resource.Info.identities(related)
            argument = Enum.find(action.arguments, &(&1.name == opts[:argument]))

            if argument && map_type?(argument.type) && !Enum.empty?(identities) do
              [{resource, action.name, opts[:argument]}]
            else
              []
            end

          _ ->
            []
        end)
      end)
    end)
    |> Enum.group_by(
      fn {resource, action, _} ->
        {resource, action}
      end,
      &elem(&1, 2)
    )
    |> Enum.map_join("\n\n", fn {{resource, action}, args} ->
      "#{inspect(resource)}.#{action}:\n" <>
        Enum.map_join(args, "\n", fn arg ->
          "* #{arg}"
        end)
    end)
  end

  defp map_type?({:array, type}) do
    map_type?(type)
  end

  defp map_type?(:map), do: true
  defp map_type?(Ash.Type.Map), do: true

  defp map_type?(type) do
    if Ash.Type.embedded_type?(type) do
      if is_atom(type) && :erlang.function_exported(type, :admin_map_type?, 0) do
        type.admin_map_type?()
      else
        false
      end
    else
      false
    end
  end

  @doc """
  Gets the resources of an Api module. Can be used at compile time.

  Liberal use of this can greatly increase compile times, or even cause compiler deadlocks.
  Use with care.
  """
  @spec depend_on_resources(Macro.t()) :: Macro.t()
  defmacro depend_on_resources(api) do
    quote do
      Code.ensure_compiled!(unquote(api))

      if registry = Ash.Api.Info.registry(unquote(api)) do
        Code.ensure_compiled!(registry)
        # I guess I have to do this because the module name is dynamic?
        # Still, seems pretty strange. This works, at least.
        Kernel.LexicalTracker.remote_dispatch(__ENV__.lexical_tracker, unquote(api), :compile)
      end

      for resource <- Ash.Api.Info.resources(unquote(api)) do
        Code.ensure_compiled!(resource)
        # same note as above
        Kernel.LexicalTracker.remote_dispatch(__ENV__.lexical_tracker, resource, :compile)
        resource
      end
    end
  end

  @doc """
  The description of the api
  """
  @spec description(Spark.Dsl.t() | Ash.Api.t()) :: String.t() | nil
  def description(api) do
    Extension.get_opt(api, [:api], :description, nil)
  end

  @doc "The resource registry for an api"
  @spec registry(Ash.Api.t()) :: atom | nil
  def registry(api) do
    Extension.get_opt(api, [:resources], :registry, nil, true)
  end

  @doc "The allow MFA for an api"
  @spec allow(Ash.Api.t() | Spark.Dsl.t()) :: mfa() | nil
  def allow(api) do
    Extension.get_opt(api, [:resources], :allow, nil, true)
  end

  @doc "The execution timeout for an api"
  @spec timeout(Ash.Api.t()) :: nil | :infinity | integer()
  def timeout(api) do
    Extension.get_opt(api, [:execution], :timeout, 30_000, true)
  end

  @doc "The short name for an api"
  @spec short_name(Ash.Api.t()) :: atom
  def short_name(api) do
    Extension.get_opt(api, [:execution], :short_name, nil) || api.default_short_name()
  end

  @doc "The trace name for an api"
  @spec trace_name(Ash.Api.t()) :: String.t()
  def trace_name(api) do
    Extension.get_opt(api, [:execution], :trace_name, nil) || to_string(short_name(api))
  end

  @doc "The span_name for an api and resource combination"
  @spec span_name(Ash.Api.t(), Ash.Resource.t(), action :: atom | binary()) :: String.t()
  def span_name(api, resource, action) do
    "#{trace_name(api)}:#{Ash.Resource.Info.trace_name(resource)}.#{action}"
  end

  @doc "Names a telemetry event for a given api/resource combo"
  @spec telemetry_event_name(Ash.Api.t(), atom | list(atom)) :: list(atom)
  def telemetry_event_name(api, name) do
    List.flatten([:ash, short_name(api), name])
  end

  @doc "Whether or not the actor is always required for an api"
  @spec require_actor?(Ash.Api.t()) :: boolean
  def require_actor?(api) do
    Extension.get_opt(api, [:authorization], :require_actor?, false, true)
  end

  @doc "When authorization should happen for a given api"
  @spec authorize(Ash.Api.t()) :: :always | :by_default | :when_requested
  def authorize(api) do
    Extension.get_opt(api, [:authorization], :authorize, :by_default, true)
  end

  @doc "Whether or not the api allows unregistered resources to be used with it"
  @spec allow_unregistered?(Ash.Api.t() | Spark.Dsl.t()) :: atom | nil
  def allow_unregistered?(api) do
    Extension.get_opt(api, [:resources], :allow_unregistered?, nil)
  end

  @doc """
  Returns `{:ok, resource}` if the resource can be used by the api, or `{:error, error}`.
  """
  @spec resource(Ash.Api.t() | Spark.Dsl.t(), Ash.Resource.t()) ::
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
          if Ash.Resource.Info.resource?(resource) do
            api =
              case api do
                api when is_atom(api) -> api
                dsl -> Extension.get_persisted(dsl, :module)
              end

            {:error, ResourceNotAllowed.exception(api: api, resource: resource)}
          else
            {:error, NoSuchResource.exception(resource: resource)}
          end
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
