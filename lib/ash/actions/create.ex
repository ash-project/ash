defmodule Ash.Actions.Create do
  alias Ash.Authorization.Authorizer
  alias Ash.Actions.ChangesetHelpers

  @spec run(Ash.api(), Ash.resource(), Ash.action(), Ash.params()) ::
          {:ok, Ash.record()} | {:error, Ecto.Changeset.t()} | {:error, Ash.error()}
  def run(api, resource, action, params) do
    if Keyword.get(params, :side_load, []) in [[], nil] do
      with %{valid?: true} = changeset <- prepare_create_params(api, resource, params),
           {:ok, %{data: created} = state} <-
             do_authorized(changeset, params, action, resource, api) do
        {:ok, Ash.Actions.Relationships.add_relationships_to_result(resource, created, state)}
      else
        %Ecto.Changeset{} = changeset ->
          {:error, changeset}

        {:error, error} ->
          {:error, error}
      end
    else
      {:error, "Cannot side load on create currently"}
    end
  end

  defp do_authorized(changeset, params, action, resource, api) do
    create_authorization_request =
      Ash.Authorization.Request.new(
        api: api,
        authorization_steps: action.authorization_steps,
        resource: resource,
        changeset: changeset,
        action_type: action.type,
        fetcher: fn ->
          do_create(resource, changeset)
        end,
        state_key: :data,
        must_fetch?: true,
        relationship: [],
        source: "#{action.type} - `#{action.name}`"
      )

    attribute_requests =
      resource
      |> Ash.attributes()
      |> Enum.reject(fn attribute ->
        attribute.primary_key?
      end)
      |> Enum.reject(fn attribute ->
        attribute.name in Map.get(changeset, :__ash_skip_authorization_fields__, [])
      end)
      |> Enum.filter(fn attribute ->
        attribute.authorization_steps != false && Map.has_key?(changeset.changes, attribute.name)
      end)
      |> Enum.map(fn attribute ->
        Ash.Authorization.Request.new(
          api: api,
          authorization_steps: attribute.authorization_steps,
          resource: resource,
          changeset: changeset,
          action_type: action.type,
          fetcher: fn -> :ok end,
          state_key: :data,
          relationship: [],
          source: "change on `#{attribute.name}`"
        )
      end)

    case Ash.Actions.Relationships.relationship_change_authorizations(api, resource, changeset) do
      {:ok, relationship_auths} ->
        if params[:authorization] do
          strict_access? =
            case Keyword.fetch(params[:authorization], :strict_access?) do
              {:ok, value} -> value
              :error -> true
            end

          Authorizer.authorize(
            params[:authorization][:user],
            [create_authorization_request | attribute_requests] ++ relationship_auths,
            strict_access?: strict_access?,
            log_final_report?: params[:authorization][:log_final_report?] || false
          )
        else
          authorization = params[:authorization] || []

          Authorizer.authorize(
            authorization[:user],
            [create_authorization_request | attribute_requests],
            fetch_only?: true
          )
        end

      {:error, error} ->
        {:error, error}
    end
  end

  defp do_create(resource, changeset) do
    if Ash.data_layer_can?(resource, :transact) do
      Ash.data_layer(resource).transaction(fn ->
        with %{valid?: true} = changeset <- ChangesetHelpers.run_before_changes(changeset),
             {:ok, result} <- Ash.DataLayer.create(resource, changeset) do
          ChangesetHelpers.run_after_changes(changeset, result)
        end
      end)
    else
      with %{valid?: true} = changeset <- ChangesetHelpers.run_before_changes(changeset),
           {:ok, result} <- Ash.DataLayer.create(resource, changeset) do
        ChangesetHelpers.run_after_changes(changeset, result)
      end
    end
  end

  defp prepare_create_params(api, resource, params) do
    attributes = Keyword.get(params, :attributes, %{})
    relationships = Keyword.get(params, :relationships, %{})

    old_relationships =
      Enum.reduce(relationships, %{}, fn {key, value}, acc ->
        if Ash.relationship(resource, key).type == :has_many do
          acc
        else
          Map.put(acc, key, value)
        end
      end)

    authorization = Keyword.get(params, :authorization, false)

    case prepare_create_attributes(resource, attributes) do
      %{valid?: true} = changeset ->
        # TODO: __ash_api__ should be unnecessary in the new way.
        # TODO: If you are saying to `add` somethign to a to_one relationship
        # but not removing the old thing, that should be a validation error
        # assuming you were authorized to read the original data.
        # If you are changing a foreign key, that needs to map to a relationship update
        changeset = Map.put(changeset, :__ash_api__, api)

        changeset =
          relationships
          |> Enum.reduce(changeset, fn {key, value}, changeset ->
            case Ash.relationship(resource, key) do
              # TODO remove the `type` checks here
              # TODO: ew
              %{cardinality: :many, type: :has_many, destination: destination, name: name} ->
                case Ash.Actions.PrimaryKeyHelpers.values_to_primary_key_filters(
                       destination,
                       value
                     ) do
                  {:ok, values} ->
                    Map.update!(changeset, :__ash_relationships__, fn ash_relationships ->
                      Map.put(ash_relationships, key, %{add: values})
                    end)

                  {:error, _error} ->
                    Ecto.Changeset.add_error(changeset, name, "Invalid Identifiers")
                end

              %{
                cardinality: :one,
                type: :belongs_to,
                destination: destination,
                name: name,
                source_field: source_field,
                destination_field: destination_field
              } ->
                case Ash.Actions.PrimaryKeyHelpers.value_to_primary_key_filter(destination, value) do
                  {:ok, value} ->
                    changeset
                    |> Map.update!(:__ash_relationships__, fn ash_relationships ->
                      Map.put(ash_relationships, key, %{add: value})
                    end)
                    # Does this assumption hold?
                    |> Ecto.Changeset.put_change(
                      source_field,
                      Keyword.fetch!(value, destination_field)
                    )
                    |> Map.put_new(:__ash_skip_authorization_fields__, [])
                    |> Map.update!(:__ash_skip_authorization_fields__, fn fields ->
                      [source_field | fields]
                    end)

                  {:error, _error} ->
                    Ecto.Changeset.add_error(changeset, name, "Invalid Identifier(s)")
                end

              _ ->
                Ecto.Changeset.add_error(changeset, key, "No such relationship")
            end
          end)

        ChangesetHelpers.prepare_relationship_changes(
          changeset,
          resource,
          old_relationships,
          authorization
        )

      changeset ->
        changeset
    end
  end

  defp prepare_create_attributes(resource, attributes) do
    allowed_keys =
      resource
      |> Ash.attributes()
      |> Enum.map(& &1.name)

    # TODO: Reject any changes for attributes that are the source field of any `belongs_to` relationships!
    attributes_with_defaults =
      resource
      |> Ash.attributes()
      |> Enum.filter(&(not is_nil(&1.default)))
      |> Enum.reduce(attributes, fn attr, attributes ->
        if Map.has_key?(attributes, attr.name) do
          attributes
        else
          Map.put(attributes, attr.name, default(attr))
        end
      end)

    changeset =
      resource
      |> struct()
      |> Ecto.Changeset.cast(attributes_with_defaults, allowed_keys)
      |> Map.put(:action, :create)
      |> Map.put(:__ash_relationships__, %{})

    resource
    |> Ash.attributes()
    |> Enum.reject(&Map.get(&1, :allow_nil?))
    |> Enum.reduce(changeset, fn attr, changeset ->
      case Ecto.Changeset.get_field(changeset, attr.name) do
        nil -> Ecto.Changeset.add_error(changeset, attr.name, "must not be nil")
        _value -> changeset
      end
    end)
  end

  defp default(%{default: {:constant, value}}), do: value
  defp default(%{default: {mod, func}}), do: apply(mod, func, [])
  defp default(%{default: function}), do: function.()
end
