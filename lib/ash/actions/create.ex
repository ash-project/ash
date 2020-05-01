defmodule Ash.Actions.Create do
  alias Ash.Engine
  alias Ash.Actions.{Attributes, Relationships, SideLoad}
  require Logger

  def run(api, resource, action, params) do
    attributes = Keyword.get(params, :attributes, %{})
    side_loads = Keyword.get(params, :side_load, [])
    side_load_filter = Keyword.get(params, :side_load_filter)
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
         %{valid?: true} = changeset <- changeset(api, resource, params),
         {:ok, side_load_requests} <-
           SideLoad.requests(api, resource, side_loads, side_load_filter, :create),
         %{
           data: %{data: %{data: %^resource{} = created}} = state,
           errors: errors
         }
         when errors == %{} <-
           do_authorized(changeset, params, action, resource, api, side_load_requests) do
      {:ok, SideLoad.attach_side_loads(created, state)}
    else
      %Ecto.Changeset{} = changeset ->
        {:error, changeset}

      %{errors: errors} when errors != %{} ->
        {:error, errors}

      {:error, error} ->
        {:error, error}
    end
  end

  def changeset(api, resource, params) do
    attributes = Keyword.get(params, :attributes, %{})
    relationships = Keyword.get(params, :relationships, %{})

    resource
    |> prepare_create_attributes(attributes)
    |> Relationships.handle_relationship_changes(api, relationships, :create)
  end

  defp do_authorized(changeset, params, action, resource, api, side_load_requests) do
    relationships = Keyword.get(params, :relationships, %{})

    create_request =
      Ash.Engine.Request.new(
        api: api,
        rules: action.rules,
        resource: resource,
        changeset:
          Relationships.changeset(
            changeset,
            api,
            relationships
          ),
        action_type: action.type,
        strict_access?: false,
        data:
          Ash.Engine.Request.resolve(
            [[:data, :changeset]],
            fn %{data: %{changeset: changeset}} ->
              resource
              |> Ash.DataLayer.create(changeset)
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

                {:error, error} ->
                  {:error, error}
              end
            end
          ),
        resolve_when_fetch_only?: true,
        path: [:data],
        name: "#{action.type} - `#{action.name}`"
      )

    attribute_requests = Attributes.attribute_change_requests(changeset, api, resource, action)

    relationship_read_requests = Map.get(changeset, :__requests__, [])

    relationship_change_requests =
      Relationships.relationship_change_requests(
        changeset,
        api,
        resource,
        action,
        relationships
      )

    if params[:authorization] do
      Engine.run(
        [create_request | attribute_requests] ++
          relationship_read_requests ++ relationship_change_requests ++ side_load_requests,
        api,
        user: params[:authorization][:user],
        bypass_strict_access?: params[:bypass_strict_access?],
        verbose?: params[:verbose?]
      )
    else
      Engine.run(
        [create_request | attribute_requests] ++
          relationship_read_requests ++ relationship_change_requests ++ side_load_requests,
        api,
        fetch_only?: true,
        verbose?: params[:verbose?]
      )
    end
  end

  defp prepare_create_attributes(resource, attributes) do
    allowed_keys =
      resource
      |> Ash.attributes()
      |> Enum.map(& &1.name)

    {attributes_with_defaults, unwriteable_attributes} =
      resource
      |> Ash.attributes()
      |> Enum.reduce({%{}, []}, fn attribute, {new_attributes, unwriteable_attributes} ->
        cond do
          !attribute.writeable? && is_nil(attribute.default) ->
            {new_attributes, unwriteable_attributes}

          !attribute.writeable? ->
            {new_attributes, [attribute | unwriteable_attributes]}

          is_nil(attribute.default) ->
            case fetch_attr(attributes, attribute.name) do
              {:ok, value} ->
                {Map.put(new_attributes, attribute.name, value), unwriteable_attributes}

              :error ->
                {new_attributes, unwriteable_attributes}
            end

          true ->
            {Map.put(attributes, attribute.name, default(attribute)), unwriteable_attributes}
        end
      end)

    changeset =
      resource
      |> struct()
      |> Ecto.Changeset.cast(attributes_with_defaults, allowed_keys)
      |> Map.put(:action, :create)
      |> Map.put(:__ash_relationships__, %{})

    changeset =
      Enum.reduce(
        unwriteable_attributes,
        changeset,
        &Ecto.Changeset.add_error(&2, &1, "attribute is not writeable")
      )

    resource
    |> Ash.attributes()
    |> Enum.reject(&Map.get(&1, :allow_nil?))
    |> Enum.reject(&Map.get(&1, :generated?))
    |> Enum.reject(&Map.get(&1, :default))
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

  defp fetch_attr(map, name) do
    case Map.fetch(map, name) do
      {:ok, value} ->
        value

      :error ->
        Map.fetch(map, to_string(name))
    end
  end
end
