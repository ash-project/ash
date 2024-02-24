defmodule Ash.Domain.Info do
  @moduledoc "Introspection tools for Ash.Domain"

  alias Ash.Error.Invalid.{NoSuchResource, ResourceNotAllowed}

  alias Spark.Dsl.Extension

  @doc """
  Gets the resources of a domain module. DO NOT USE AT COMPILE TIME.

  If you need the resource list at compile time, use `depend_on_resources/1`
  """
  @spec resources(Ash.Domain.t()) :: list(Ash.Resource.t())
  def resources(domain) do
    domain
    |> Extension.get_entities([:resources])
    |> Enum.map(& &1.resource)
  end

  def find_manage_relationships_with_identity_not_configured(otp_app) do
    otp_app
    |> Application.get_env(:ash_domains, [])
    |> Enum.flat_map(&Ash.Domain.Info.resources/1)
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
  Gets the resources of a domain module. Can be used at compile time.

  Liberal use of this can greatly increase compile times, or even cause compiler deadlocks.
  Use with care.
  """
  @spec depend_on_resources(Macro.t()) :: Macro.t()

  defmacro depend_on_resources(domain) do
    quote do
      Code.ensure_compiled!(unquote(domain))

      for resource <- Ash.Domain.Info.resources(unquote(domain)) do
        Code.ensure_compiled!(resource)
        # same note as above
        Kernel.LexicalTracker.remote_dispatch(__ENV__.lexical_tracker, resource, :compile)
        resource
      end
    end
  end

  @doc """
  The description of the domain
  """
  @spec description(Spark.Dsl.t() | Ash.Domain.t()) :: String.t() | nil
  def description(domain) do
    Extension.get_opt(domain, [:domain], :description, nil)
  end

  @doc "The allow MFA for an domain"
  @spec allow(Ash.Domain.t() | Spark.Dsl.t()) :: mfa() | nil
  def allow(domain) do
    Extension.get_opt(domain, [:resources], :allow, nil, true)
  end

  @doc "The execution timeout for an domain"
  @spec timeout(Ash.Domain.t()) :: nil | :infinity | integer()
  def timeout(domain) do
    Extension.get_opt(domain, [:execution], :timeout, 30_000, true)
  end

  @doc "The short name for an domain"
  @spec short_name(Ash.Domain.t()) :: atom
  def short_name(domain) do
    Extension.get_opt(domain, [:execution], :short_name, nil) || domain.default_short_name()
  end

  @doc "The trace name for an domain"
  @spec trace_name(Ash.Domain.t()) :: String.t()
  def trace_name(domain) do
    Extension.get_opt(domain, [:execution], :trace_name, nil) || to_string(short_name(domain))
  end

  @doc "The span_name for an domain and resource combination"
  @spec span_name(Ash.Domain.t(), Ash.Resource.t(), action :: atom | binary()) :: String.t()
  def span_name(domain, resource, action) do
    "#{trace_name(domain)}:#{Ash.Resource.Info.trace_name(resource)}.#{action}"
  end

  @doc "Names a telemetry event for a given domain/resource combo"
  @spec telemetry_event_name(Ash.Domain.t(), atom | list(atom)) :: list(atom)
  def telemetry_event_name(domain, name) do
    List.flatten([:ash, short_name(domain), name])
  end

  @doc "Whether or not the actor is always required for an domain"
  @spec require_actor?(Ash.Domain.t()) :: boolean
  def require_actor?(domain) do
    Extension.get_opt(domain, [:authorization], :require_actor?, false, true)
  end

  @doc "When authorization should happen for a given domain"
  @spec authorize(Ash.Domain.t()) :: :always | :by_default | :when_requested
  def authorize(domain) do
    Extension.get_opt(domain, [:authorization], :authorize, :by_default, true)
  end

  @doc "Whether or not the domain allows unregistered resources to be used with it"
  @spec allow_unregistered?(Ash.Domain.t() | Spark.Dsl.t()) :: atom | nil
  def allow_unregistered?(domain) do
    Extension.get_opt(domain, [:resources], :allow_unregistered?, nil)
  end

  @doc """
  Returns `{:ok, resource}` if the resource can be used by the domain, or `{:error, error}`.
  """
  @spec resource(Ash.Domain.t() | Spark.Dsl.t(), Ash.Resource.t()) ::
          {:ok, Ash.Resource.t()} | {:error, Ash.Error.t()}
  def resource(domain, resource) do
    cond do
      allow_unregistered?(domain) ->
        if Spark.Dsl.is?(resource, Ash.Resource) do
          resource
        else
          nil
        end

      Ash.Resource.Info.embedded?(resource) ->
        resource

      true ->
        domain
        |> resources()
        |> Enum.find(&(&1 == resource))
    end
    |> case do
      nil ->
        if allowed?(allow(domain), resource) do
          {:ok, resource}
        else
          if Ash.Resource.Info.resource?(resource) do
            domain =
              case domain do
                domain when is_atom(domain) -> domain
                dsl -> Extension.get_persisted(dsl, :module)
              end

            {:error, ResourceNotAllowed.exception(domain: domain, resource: resource)}
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
