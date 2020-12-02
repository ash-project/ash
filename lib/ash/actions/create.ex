defmodule Ash.Actions.Create do
  @moduledoc false
  alias Ash.Actions.Relationships
  alias Ash.Engine
  alias Ash.Engine.Request
  require Logger

  alias Ash.Error.Changes.{InvalidAttribute, InvalidRelationship, Required}

  @spec run(Ash.api(), Ash.changeset(), Ash.action(), Keyword.t()) ::
          {:ok, Ash.record()} | {:error, Ash.error()}
  def run(api, changeset, action, opts) do
    upsert? = opts[:upsert?] || false
    resource = changeset.resource

    engine_opts =
      opts
      |> Keyword.take([:verbose?, :actor, :authorize?])
      |> Keyword.put(:transaction?, true)

    with :ok <- validate_multitenancy(changeset),
         %{valid?: true} = changeset <- changeset(changeset, api, action, opts[:actor]),
         :ok <- check_upsert_support(changeset.resource, upsert?),
         %{
           data: %{commit: %^resource{} = created},
           errors: []
         } = engine_result <-
           do_run_requests(
             changeset,
             upsert?,
             engine_opts,
             action,
             resource,
             api
           ) do
      created
      |> add_tenant(changeset)
      |> add_notifications(engine_result, opts)
    else
      %Ash.Changeset{errors: errors} ->
        {:error, Ash.Error.to_ash_error(errors)}

      %{errors: errors} ->
        {:error, Ash.Error.to_ash_error(errors)}

      {:error, error} ->
        {:error, Ash.Error.to_ash_error(error)}
    end
  end

  defp add_tenant(data, changeset) do
    if Ash.Resource.multitenancy_strategy(changeset.resource) do
      %{data | __metadata__: Map.put(data.__metadata__, :tenant, changeset.tenant)}
    else
      data
    end
  end

  defp add_notifications(result, engine_result, opts) do
    if opts[:return_notifications?] do
      {:ok, result, Map.get(engine_result, :resource_notifications, [])}
    else
      {:ok, result}
    end
  end

  defp changeset(changeset, api, action, actor) do
    %{changeset | api: api}
    |> validate_attributes_accepted(action)
    |> validate_relationships_accepted(action)
    |> run_action_changes(action, actor)
    |> Relationships.handle_relationship_changes()
    |> set_defaults()
    |> validate_required_belongs_to()
    |> add_validations()
    |> require_values()
    |> Ash.Changeset.cast_arguments(action)
  end

  defp require_values(changeset) do
    changeset.resource
    |> Ash.Resource.attributes()
    |> Enum.reject(&(&1.allow_nil? || &1.private?))
    |> Enum.reduce(changeset, fn required_attribute, changeset ->
      if Ash.Changeset.changing_attribute?(changeset, required_attribute.name) do
        changeset
      else
        Ash.Changeset.add_error(
          changeset,
          Required.exception(field: required_attribute.name, type: :attribute)
        )
      end
    end)
  end

  defp validate_required_belongs_to(changeset) do
    changeset.resource
    |> Ash.Resource.relationships()
    |> Enum.filter(&(&1.type == :belongs_to))
    |> Enum.filter(& &1.required?)
    |> Enum.reduce(changeset, fn required_relationship, changeset ->
      case Map.fetch(changeset.relationships, required_relationship.name) do
        {:ok, %{add: adding}} when adding != nil and adding != [] ->
          changeset

        {:ok, %{replace: replacing}} when replacing != nil and replacing != [] ->
          changeset

        _ ->
          Ash.Changeset.add_error(
            changeset,
            Required.exception(
              field: required_relationship.name,
              type: :relationship
            )
          )
      end
    end)
  end

  defp run_action_changes(changeset, %{changes: changes}, actor) do
    Enum.reduce(changes, changeset, fn %{change: {module, opts}}, changeset ->
      module.change(changeset, opts, %{actor: actor})
    end)
  end

  defp validate_attributes_accepted(changeset, %{accept: nil}), do: changeset

  defp validate_attributes_accepted(changeset, %{accept: accepted_attributes}) do
    changeset.attributes
    |> Enum.reject(fn {key, _value} ->
      key in accepted_attributes
    end)
    |> Enum.reduce(changeset, fn {key, _}, changeset ->
      Ash.Changeset.add_error(
        changeset,
        InvalidAttribute.exception(field: key, message: "cannot be changed")
      )
    end)
  end

  defp validate_relationships_accepted(changeset, %{accept: nil}), do: changeset

  defp validate_relationships_accepted(changeset, %{accept: accepted_relationships}) do
    changeset.relationships
    |> Enum.reject(fn {key, _value} ->
      key in accepted_relationships
    end)
    |> Enum.reduce(changeset, fn {key, _}, changeset ->
      Ash.Changeset.add_error(
        changeset,
        InvalidRelationship.exception(
          relationship: key,
          message: "Cannot be changed"
        )
      )
    end)
  end

  defp add_validations(changeset) do
    Ash.Changeset.before_action(changeset, fn changeset ->
      changeset.resource
      |> Ash.Resource.validations(:create)
      |> Enum.reduce(changeset, fn validation, changeset ->
        if validation.expensive? and not changeset.valid? do
          changeset
        else
          do_validation(changeset, validation)
        end
      end)
    end)
  end

  defp do_validation(changeset, validation) do
    case validation.module.validate(changeset, validation.opts) do
      :ok -> changeset
      {:error, error} -> Ash.Changeset.add_error(changeset, error)
    end
  end

  defp set_defaults(changeset) do
    changeset.resource
    |> Ash.Resource.attributes()
    |> Enum.filter(&(not is_nil(&1.default)))
    |> Enum.reduce(changeset, fn attribute, changeset ->
      Ash.Changeset.force_change_new_attribute_lazy(changeset, attribute.name, fn ->
        default(attribute.default)
      end)
    end)
  end

  defp do_run_requests(
         changeset,
         upsert?,
         engine_opts,
         action,
         resource,
         api
       ) do
    authorization_request =
      Request.new(
        api: api,
        resource: resource,
        changeset: Relationships.changeset(changeset),
        action: action,
        data: nil,
        path: [:data],
        name: "#{action.type} - `#{action.name}`: prepare"
      )

    relationship_read_requests = changeset.requests

    commit_request =
      Request.new(
        api: api,
        resource: resource,
        changeset:
          Request.resolve([[:data, :changeset]], fn %{data: %{changeset: changeset}} ->
            {:ok, changeset}
          end),
        action: action,
        notify?: true,
        data:
          Request.resolve(
            [[:commit, :changeset]],
            fn %{commit: %{changeset: changeset}} ->
              changeset = set_tenant(changeset)

              Ash.Changeset.with_hooks(changeset, fn changeset ->
                if upsert? do
                  Ash.DataLayer.upsert(resource, changeset)
                else
                  Ash.DataLayer.create(resource, changeset)
                end
              end)
            end
          ),
        path: [:commit],
        name: "#{action.type} - `#{action.name}`: commit"
      )

    Engine.run(
      [authorization_request | [commit_request | relationship_read_requests]],
      api,
      engine_opts
    )
  end

  defp validate_multitenancy(changeset) do
    if Ash.Resource.multitenancy_strategy(changeset.resource) &&
         not Ash.Resource.multitenancy_global?(changeset.resource) && is_nil(changeset.tenant) do
      {:error, "#{inspect(changeset.resource)} changesets require a tenant to be specified"}
    else
      :ok
    end
  end

  defp set_tenant(changeset) do
    if changeset.tenant && Ash.Resource.multitenancy_strategy(changeset.resource) == :attribute do
      attribute = Ash.Resource.multitenancy_attribute(changeset.resource)
      {m, f, a} = Ash.Resource.multitenancy_parse_attribute(changeset.resource)
      attribute_value = apply(m, f, [changeset.tenant | a])

      changeset
      |> Ash.Changeset.force_change_attribute(attribute, attribute_value)
      |> Map.put(:tenant, nil)
    else
      changeset
    end
  end

  defp check_upsert_support(_resource, false), do: :ok

  defp check_upsert_support(resource, true) do
    if Ash.Resource.data_layer_can?(resource, :upsert) do
      :ok
    else
      {:error, {:unsupported, :upsert}}
    end
  end

  defp default({mod, func, args}), do: apply(mod, func, args)
  defp default(function) when is_function(function, 0), do: function.()
  defp default(value), do: value
end
