defmodule Ash.Actions.Update do
  alias Ash.Engine
  alias Ash.Actions.{Attributes, Relationships}

  @spec run(Ash.api(), Ash.record(), Ash.action(), Ash.params()) ::
          {:ok, Ash.record()} | {:error, Ecto.Changeset.t()} | {:error, Ash.error()}
  # TODO: To support a "read and update" pattern, we would support taking a filter instead of a record here.
  def run(api, %resource{} = record, action, params) do
    if Keyword.get(params, :side_load, []) in [[], nil] do
      Ash.DataLayer.transact(resource, fn ->
        do_run(api, record, action, params)
      end)
    else
      {:error, "Cannot side load on update currently"}
    end
  end

  defp do_run(api, %resource{} = record, action, params) do
    attributes = Keyword.get(params, :attributes, %{})
    relationships = Keyword.get(params, :relationships, %{})

    with {:ok, relationships} <-
           Relationships.validate_not_changing_relationship_and_source_field(
             relationships,
             attributes,
             resource
           ),
         {:ok, attributes, relationships} <-
           Relationships.field_changes_into_relationship_changes(
             relationships,
             attributes,
             resource
           ),
         params <- Keyword.merge(params, attributes: attributes, relationships: relationships),
         %{valid?: true} = changeset <- changeset(record, api, params),
         {:ok, %{data: updated}} <- do_authorized(changeset, params, action, resource, api) do
      {:ok, updated}
    else
      %Ecto.Changeset{} = changeset ->
        {:error, changeset}

      {:error, error} ->
        {:error, error}
    end
  end

  def changeset(record, api, params) do
    attributes = Keyword.get(params, :attributes, %{})
    relationships = Keyword.get(params, :relationships, %{})

    record
    |> prepare_update_attributes(attributes)
    |> Relationships.handle_relationship_changes(api, relationships, :update)
  end

  defp do_authorized(changeset, params, action, resource, api) do
    relationships = Keyword.get(params, :relationships)

    update_authorization_request =
      Ash.Engine.Request.new(
        api: api,
        rules: action.rules,
        changeset:
          Ash.Actions.Relationships.authorization_changeset(
            changeset,
            api,
            relationships
          ),
        action_type: action.type,
        fetcher: fn changeset, _ ->
          resource
          |> Ash.DataLayer.update(changeset)
          |> case do
            {:ok, result} ->
              changeset
              |> Map.get(:__after_changes__, [])
              |> Enum.reduce_while({:ok, result}, fn func, {:ok, result} ->
                case func.(changeset, result) do
                  {:ok, result} -> {:cont, {:ok, result}}
                  {:error, error} -> {:halt, {:error, error}}
                end
              end)
          end
        end,
        dependencies: Map.get(changeset, :__changes_depend_on__) || [],
        state_key: :data,
        must_fetch?: true,
        relationship: [],
        source: "#{action.type} - `#{action.name}`"
      )

    attribute_requests =
      Attributes.attribute_change_authorizations(changeset, api, resource, action)

    relationship_auths = Map.get(changeset, :__authorizations__, [])

    if params[:authorization] do
      strict_access? =
        case Keyword.fetch(params[:authorization], :strict_access?) do
          {:ok, value} -> value
          :error -> false
        end

      Engine.run(
        params[:authorization][:user],
        [update_authorization_request | attribute_requests] ++ relationship_auths,
        strict_access?: strict_access?,
        log_final_report?: params[:authorization][:log_final_report?] || false
      )
    else
      authorization = params[:authorization] || []

      Engine.run(
        authorization[:user],
        [update_authorization_request | attribute_requests] ++ relationship_auths,
        fetch_only?: true
      )
    end
  end

  defp prepare_update_attributes(%resource{} = record, attributes) do
    allowed_keys =
      resource
      |> Ash.attributes()
      |> Enum.map(& &1.name)

    changeset =
      record
      |> Ecto.Changeset.cast(attributes, allowed_keys)
      |> Map.put(:action, :update)

    resource
    |> Ash.attributes()
    |> Enum.reject(&Map.get(&1, :allow_nil?))
    |> Enum.reduce(changeset, fn attr, changeset ->
      case Ecto.Changeset.fetch_change(changeset, attr.name) do
        {:ok, nil} ->
          Ecto.Changeset.add_error(changeset, attr.name, "must not be nil")

        _ ->
          changeset
      end
    end)
  end
end
