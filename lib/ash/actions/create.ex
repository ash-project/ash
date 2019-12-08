defmodule Ash.Actions.Create do
  alias Ash.Authorization.Authorizer
  alias Ash.Actions.SideLoader

  def run(resource, action, api, %{authorize?: true} = params) do
    case prepare_create_params(resource, params) do
      {:ok, changeset} ->
        auth_context = %{
          resource: resource,
          action: action,
          params: params,
          changeset: changeset
        }

        user = Map.get(params, :user)

        Authorizer.authorize(user, action.rules, auth_context, fn authorize_data_fun ->
          with {:ok, result} <- Ash.DataLayer.create(resource, changeset),
               {:auth, :allow} <-
                 {:auth, authorize_data_fun.(user, result, action.rules, auth_context)} do
            side_loads = Map.get(params, :side_load, [])
            global_params = Map.take(params, [:authorize?, :user])

            SideLoader.side_load(resource, result, side_loads, api, global_params)
          else
            {:error, error} -> {:error, error}
            {:auth, _} -> {:error, :forbidden}
          end
        end)

      {:error, changeset} ->
        {:error, changeset}
    end
  end

  def run(resource, _action, api, params) do
    with {:ok, changeset} <- prepare_create_params(resource, params),
         {:ok, result} <- Ash.DataLayer.create(resource, changeset) do
      side_loads = Map.get(params, :side_load, [])
      global_params = Map.take(params, [:authorize?, :user])

      SideLoader.side_load(resource, result, side_loads, api, global_params)
    else
      {:error, error} -> {:error, error}
    end
  end

  defp prepare_create_params(resource, params) do
    attributes = Map.get(params, :attributes, %{})
    relationships = Map.get(params, :relationships, %{})

    with {:ok, %{valid?: true} = changeset} <- prepare_create_attributes(resource, attributes),
         {:ok, %{valid?: true}} <-
           prepare_create_relationships(changeset, resource, relationships) do
      {:ok, changeset}
    else
      {:error, error} ->
        {:error, error}
    end
  end

  defp prepare_create_attributes(resource, attributes) do
    allowed_keys =
      resource
      |> Ash.attributes()
      |> Enum.map(& &1.name)

    # Map.put_new(attributes, :id, Ecto.UUID.generate())

    attributes_with_defaults =
      resource
      |> Ash.attributes()
      |> Stream.filter(&(not is_nil(&1.default)))
      |> Enum.reduce(attributes, fn attr, attributes ->
        if Map.has_key?(attributes, attr.name) do
          attributes
        else
          Map.put(attributes, attr.name, default(attr))
        end
      end)

    resource
    |> struct()
    |> Ecto.Changeset.cast(attributes_with_defaults, allowed_keys)
    |> case do
      %{valid?: true} = changeset ->
        {:ok, changeset}

      _error_changeset ->
        # TODO: Print the errors here.
        {:error, "invalid attributes"}
    end
  end

  defp default(%{default: {:constant, value}}), do: value
  defp default(%{default: {mod, func}}), do: apply(mod, func, [])
  defp default(%{default: function}), do: function.()

  defp prepare_create_relationships(changeset, _resource, _relationships) do
    {:ok, changeset}
    # relationships
    # # Eventually we'll have to just copy changeset's logic
    # # and/or use it directly (now that ecto is split up, maybe thats the way to do all of this?)
    # |> Enum.reduce({%{}, []}, fn {key, value}, {changes, errors} ->
    #   case Ash.relationship(resource, key) do
    #     nil ->
    #       {changes, ["unknown attribute #{key}" | errors]}

    #     _attribute ->
    #       # TODO do actual value validation here
    #       {Map.put(changes, key, value), errors}
    #   end
    # end)
    # |> case do
    #   {changes, []} -> {:ok, changes}
    #   {_, errors} -> {:error, errors}
    # end
  end
end
