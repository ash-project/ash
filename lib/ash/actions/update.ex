defmodule Ash.Actions.Update do
  @moduledoc false
  alias Ash.Actions.Relationships
  alias Ash.Engine
  alias Ash.Engine.Request
  require Logger

  alias Ash.Error.Changes.{InvalidAttribute, InvalidRelationship}

  @spec run(Ash.api(), Ash.record(), Ash.action(), Keyword.t()) ::
          {:ok, Ash.record()} | {:error, Ash.Changeset.t()} | {:error, Ash.error()}
  def run(api, changeset, action, opts) do
    engine_opts =
      opts
      |> Keyword.take([:verbose?, :actor, :authorize?])
      |> Keyword.put(:transaction?, true)

    resource = changeset.resource

    with %{valid?: true} = changeset <- changeset(changeset, api, action, opts[:actor]),
         %{data: %{commit: %^resource{} = updated}, errors: []} <-
           do_run_requests(
             changeset,
             engine_opts,
             action,
             resource,
             api
           ) do
      {:ok, updated}
    else
      %Ash.Changeset{errors: errors} ->
        {:error, Ash.Error.to_ash_error(errors)}

      %{errors: errors} ->
        {:error, Ash.Error.to_ash_error(errors)}

      {:error, error} ->
        {:error, Ash.Error.to_ash_error(error)}
    end
  end

  defp changeset(changeset, api, action, actor) do
    %{changeset | api: api}
    |> validate_attributes_accepted(action)
    |> validate_relationships_accepted(action)
    |> run_action_changes(action, actor)
    |> Relationships.handle_relationship_changes()
    |> set_defaults()
    |> add_validations()
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
        InvalidAttribute.exception(field: key, message: "Cannot be changed")
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
    changeset.resource
    |> Ash.Resource.validations(changeset.action_type)
    |> Enum.reduce(changeset, fn validation, changeset ->
      Ash.Changeset.before_action(changeset, &do_validation(&1, validation))
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
    |> Enum.filter(&(not is_nil(&1.update_default)))
    |> Enum.reduce(changeset, fn attribute, changeset ->
      Ash.Changeset.change_new_attribute_lazy(changeset, attribute.name, fn ->
        default(attribute.update_default)
      end)
    end)
  end

  defp do_run_requests(
         changeset,
         engine_opts,
         action,
         resource,
         api
       ) do
    authorization_request =
      Request.new(
        api: api,
        changeset: Relationships.changeset(changeset),
        action: action,
        resource: resource,
        data: changeset.data,
        path: :data,
        name: "#{action.type} - `#{action.name}`: prepare"
      )

    commit_request =
      Request.new(
        api: api,
        changeset:
          Request.resolve([[:data, :changeset]], fn %{data: %{changeset: changeset}} ->
            {:ok, changeset}
          end),
        action: action,
        resource: resource,
        data:
          Request.resolve(
            [[:data, :changeset]],
            fn %{data: %{changeset: changeset}} ->
              Ash.Changeset.with_hooks(changeset, fn changeset ->
                Ash.DataLayer.update(resource, changeset)
              end)
            end
          ),
        path: [:commit],
        name: "#{action.type} - `#{action.name}` commit"
      )

    relationship_requests = changeset.requests

    Engine.run(
      [authorization_request | [commit_request | relationship_requests]],
      api,
      engine_opts
    )
  end

  defp default({mod, func, args}), do: apply(mod, func, args)
  defp default(function) when is_function(function, 0), do: function.()
  defp default(value), do: value
end
