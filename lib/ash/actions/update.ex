defmodule Ash.Actions.Update do
  alias Ash.Authorization.Authorizer
  alias Ash.Actions.SideLoader
  alias Ash.Actions.ChangesetHelpers

  @spec run(Ash.api(), Ash.record(), Ash.action(), Ash.params()) ::
          {:ok, Ash.record()} | {:error, Ecto.Changeset.t()} | {:error, Ash.error()}
  def run(api, %resource{} = record, action, %{authorize?: true} = params) do
    case prepare_update_params(api, record, params) do
      %Ecto.Changeset{valid?: true} = changeset ->
        auth_context = %{
          resource: resource,
          action: action,
          params: params,
          changeset: changeset
        }

        user = Map.get(params, :user)

        Authorizer.authorize(
          user,
          action.authorization_steps,
          auth_context,
          fn authorize_data_fun ->
            with {:ok, result} <- do_update(resource, changeset),
                 {:auth, :allow} <-
                   {:auth,
                    authorize_data_fun.(user, result, action.authorization_steps, auth_context)} do
              side_loads = Map.get(params, :side_load, [])
              global_params = Map.take(params, [:authorize?, :user])

              SideLoader.side_load(resource, result, side_loads, api, global_params)
            else
              {:error, error} -> {:error, error}
              {:auth, _} -> {:error, :forbidden}
            end
          end
        )

      changeset ->
        {:error, changeset}
    end
  end

  def run(api, %resource{} = record, _action, params) do
    with %Ecto.Changeset{valid?: true} = changeset <- prepare_update_params(api, record, params),
         %Ecto.Changeset{valid?: true} = changeset <-
           ChangesetHelpers.run_before_changes(changeset),
         {:ok, result} <- Ash.DataLayer.update(resource, changeset),
         {:ok, result} <- ChangesetHelpers.run_after_changes(changeset, result) do
      side_loads = Map.get(params, :side_load, [])
      global_params = Map.take(params, [:authorize?, :user])

      SideLoader.side_load(resource, result, side_loads, api, global_params)
    else
      {:error, error} -> {:error, error}
      %Ecto.Changeset{} = changeset -> {:error, changeset}
    end
  end

  defp do_update(resource, changeset) do
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

  defp prepare_update_params(api, %resource{} = record, params) do
    attributes = Map.get(params, :attributes, %{})
    relationships = Map.get(params, :relationships, %{})
    authorize? = Map.get(params, :authorize?, false)
    user = Map.get(params, :user)

    with %{valid?: true} = changeset <- prepare_update_attributes(record, attributes),
         changeset <- Map.put(changeset, :__ash_api__, api) do
      prepare_update_relationships(changeset, resource, relationships, authorize?, user)
    end
  end

  defp prepare_update_attributes(%resource{} = record, attributes) do
    allowed_keys =
      resource
      |> Ash.attributes()
      |> Enum.map(& &1.name)

    Ecto.Changeset.cast(record, attributes, allowed_keys)
  end

  defp prepare_update_relationships(changeset, resource, relationships, authorize?, user) do
    Enum.reduce(relationships, changeset, fn {relationship, value}, changeset ->
      case Ash.relationship(resource, relationship) do
        # %{type: :belongs_to, source_field: source_field} ->
        #   belongs_to_assoc_update(changeset, source_field, value)

        %{type: :has_one} = rel ->
          ChangesetHelpers.has_one_assoc_update(changeset, rel, value, authorize?, user)

        # %{type: :has_many} = rel ->
        #   has_many_assoc_update(changeset, rel, value)

        # %{type: :many_to_many} = rel ->
        #   many_to_many_assoc_update(changeset, rel, value, repo)

        _ ->
          Ecto.Changeset.add_error(changeset, relationship, "No such relationship")
      end
    end)
  end
end
