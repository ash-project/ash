defmodule Ash.Actions.Update do
  alias Ash.Authorization.Authorizer
  alias Ash.Actions.SideLoad
  alias Ash.Actions.ChangesetHelpers

  @spec run(Ash.api(), Ash.record(), Ash.action(), Ash.params()) ::
          {:ok, Ash.record()} | {:error, Ecto.Changeset.t()} | {:error, Ash.error()}
  def run(api, %resource{} = record, action, params) do
    if Keyword.get(params, :side_load, []) in [[], nil] do
      case prepare_update_params(api, record, params) do
        %Ecto.Changeset{valid?: true} = changeset ->
          user = Keyword.get(params, :user)

          with {:auth, :authorized} <-
                 {:auth, do_authorize(params, action, user, resource, changeset)},
               %Ecto.Changeset{valid?: true} = changeset <-
                 prepare_update_params(api, record, params),
               %Ecto.Changeset{valid?: true} = changeset <-
                 ChangesetHelpers.run_before_changes(changeset),
               {:ok, result} <- do_update(resource, changeset) do
            ChangesetHelpers.run_after_changes(changeset, result)
          else
            :forbidden -> {:error, :forbidden}
            {:error, error} -> {:error, error}
            %Ecto.Changeset{} = changeset -> {:error, changeset}
          end

        changeset ->
          {:error, changeset}
      end
    else
      {:error, "Cannot side load on update currently"}
    end
  end

  defp do_authorize(params, action, user, resource, changeset) do
    if Keyword.get(params, :authorize?, false) do
      auth_request =
        Ash.Authorization.Request.new(
          resource: resource,
          rules: action.rules,
          changeset: changeset
        )

      Authorizer.authorize(user, [auth_request])
    else
      :authorized
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
      else
        %Ecto.Changeset{valid?: false} = changeset ->
          {:error, changeset}
      end
    end
  end

  defp prepare_update_params(api, %resource{} = record, params) do
    attributes = Keyword.get(params, :attributes, %{})
    relationships = Keyword.get(params, :relationships, %{})
    authorize? = Keyword.get(params, :authorize?, false)
    user = Keyword.get(params, :user)

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
        %{type: :belongs_to} = rel ->
          ChangesetHelpers.belongs_to_assoc_update(changeset, rel, value, authorize?, user)

        %{type: :has_one} = rel ->
          ChangesetHelpers.has_one_assoc_update(changeset, rel, value, authorize?, user)

        %{type: :has_many} = rel ->
          ChangesetHelpers.has_many_assoc_update(changeset, rel, value, authorize?, user)

        # %{type: :many_to_many} = rel ->
        #   ChangesetHelpers.many_to_many_assoc_update(changeset, rel, value, authorize?, user)

        _ ->
          Ecto.Changeset.add_error(changeset, relationship, "No such relationship")
      end
    end)
  end
end
