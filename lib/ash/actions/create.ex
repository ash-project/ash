defmodule Ash.Actions.Create do
  alias Ash.Engine
  alias Ash.Actions.{Relationships, SideLoad}
  require Logger

  def run(api, resource, action, params) do
    attributes = Keyword.get(params, :attributes, %{})
    relationships = Keyword.get(params, :relationships, %{})

    action =
      if is_atom(action) and not is_nil(action) do
        Ash.action(resource, action, :read)
      else
        action
      end

    with {:ok, side_load_query} <- side_loads_as_query(api, resource, params[:side_load]),
         {:ok, relationships} <-
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
         %{valid?: true} = changeset <-
           changeset(api, resource, params),
         side_load_requests <-
           SideLoad.requests(side_load_query, api.query(resource)),
         %{
           data: %{data: %^resource{} = created} = state,
           errors: []
         } <-
           do_run_requests(changeset, params, action, resource, api, side_load_requests) do
      {:ok, SideLoad.attach_side_loads(created, state)}
    else
      %Ecto.Changeset{} = changeset ->
        {:error, Ash.Error.Changeset.changeset_to_errors(resource, changeset)}

      %Ash.Engine{errors: errors} ->
        {:error, Ash.to_ash_error(errors)}

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

  defp do_run_requests(changeset, params, action, resource, api, side_load_requests) do
    case resolve_data(resource, params) do
      {:error, error} ->
        {:error, error}

      {:ok, data_resolver} ->
        relationships = Keyword.get(params, :relationships, %{})

        create_request =
          Ash.Engine.Request.new(
            api: api,
            resource: resource,
            changeset:
              Relationships.changeset(
                changeset,
                api,
                relationships
              ),
            action: action,
            data: data_resolver,
            request_id: :change,
            path: [:data],
            name: "#{action.type} - `#{action.name}`"
          )

        relationship_read_requests = Map.get(changeset, :__requests__, [])

        Engine.run(
          [create_request | relationship_read_requests] ++ side_load_requests,
          api,
          verbose?: params[:verbose?]
        )
    end
  end

  defp resolve_data(resource, params) do
    case check_upsert_support(resource, params) do
      :ok ->
        {:ok,
         Ash.Engine.Request.resolve(
           [[:data, :changeset]],
           fn %{data: %{changeset: changeset}} ->
             result =
               if params[:upsert?] do
                 Ash.DataLayer.upsert(resource, changeset)
               else
                 Ash.DataLayer.create(resource, changeset)
               end

             case result do
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
         )}

      {:error, error} ->
        {:error, error}
    end
  end

  defp check_upsert_support(resource, params) do
    cond do
      !params[:upsert?] ->
        :ok

      Ash.data_layer_can?(resource, :upsert) ->
        :ok

      true ->
        {:error, {:unsupported, :upsert}}
    end
  end

  defp side_loads_as_query(_api, _resource, nil), do: {:ok, nil}
  defp side_loads_as_query(_api, _resource, %Ash.Query{errors: []} = query), do: {:ok, query}
  defp side_loads_as_query(_api, _resource, %Ash.Query{errors: errors}), do: {:error, errors}

  defp side_loads_as_query(api, resource, side_loads) when is_list(side_loads) do
    resource
    |> api.query()
    |> Ash.Query.side_load(side_loads)
    |> case do
      %{errors: []} = query -> {:ok, query}
      %{errors: errors} -> {:error, errors}
    end
  end

  defp prepare_create_attributes(resource, attributes) do
    allowed_keys =
      resource
      |> Ash.attributes()
      |> Enum.map(& &1.name)

    {attributes_with_defaults, unwritable_attributes} =
      resource
      |> Ash.attributes()
      |> Enum.reduce({%{}, []}, fn attribute, {new_attributes, unwritable_attributes} ->
        provided_value = fetch_attr(attributes, attribute.name)
        provided? = match?({:ok, _}, provided_value)

        cond do
          provided? && !attribute.writable? ->
            {new_attributes, [attribute | unwritable_attributes]}

          provided? ->
            {:ok, value} = provided_value
            {Map.put(new_attributes, attribute.name, value), unwritable_attributes}

          is_nil(attribute.default) ->
            {new_attributes, unwritable_attributes}

          true ->
            {Map.put(new_attributes, attribute.name, default(attribute)), unwritable_attributes}
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
        unwritable_attributes,
        changeset,
        &Ecto.Changeset.add_error(&2, &1.name, "attribute is not writable")
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
        {:ok, value}

      :error ->
        Map.fetch(map, to_string(name))
    end
  end
end
