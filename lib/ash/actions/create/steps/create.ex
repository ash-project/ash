defmodule Ash.Actions.Create.Steps.Create do
  use Reactor.Step

  def run(%{changeset: changeset}, _, _) do
    actor = changeset.context.private.actor
    authorize? = changeset.context.private.authorize?

    changeset
    |> Ash.Changeset.with_hooks(
      fn changeset ->
        case Ash.Actions.ManagedRelationships.setup_managed_belongs_to_relationships(
               changeset,
               actor,
               authorize?: authorize?,
               actor: actor
             ) do
          {:error, error} ->
            {:error, Ash.Changeset.add_error(changeset, error)}

          {changeset, manage_instructions} ->
            changeset =
              if changeset.context[:private][:action_result] do
                changeset
              else
                Ash.Changeset.require_values(
                  changeset,
                  :create
                )
                |> Ash.Changeset.require_values(
                  :update,
                  false,
                  changeset.action.require_attributes
                )
              end

            if changeset.valid? do
              cond do
                changeset.action.manual ->
                  {mod, opts} = changeset.action.manual

                  if result = changeset.context[:private][:action_result] do
                    result
                  else
                    mod.create(changeset, opts, %{
                      actor: actor,
                      tenant: changeset.tenant,
                      authorize?: authorize?,
                      api: changeset.api
                    })
                  end
                  |> set_result_tenant_metadata(changeset)
                  |> manage_relationships(changeset)

                true ->
                  belongs_to_attrs =
                    changeset.resource
                    |> Ash.Resource.Info.relationships()
                    |> Enum.filter(&(&1.type == :belongs_to))
                    |> Enum.map(& &1.source_attribute)

                  final_check =
                    changeset.resource
                    |> Ash.Resource.Info.attributes()
                    |> Enum.reject(
                      &(&1.allow_nil? || &1.generated? || &1.name in belongs_to_attrs)
                    )

                  changeset =
                    if changeset.context[:private][:action_result] do
                      changeset
                    else
                      changeset =
                        changeset
                        |> Ash.Changeset.require_values(
                          :create,
                          true,
                          final_check
                        )

                      {changeset, _} =
                        Ash.Actions.ManagedRelationships.validate_required_belongs_to(
                          {changeset, []},
                          false
                        )

                      changeset
                    end

                  if changeset.valid? do
                    cond do
                      result = changeset.context[:private][:action_result] ->
                        result
                        |> set_result_tenant_metadata(changeset)
                        |> manage_relationships(changeset)

                      changeset.context.private.upsert? ->
                        changeset.resource
                        |> Ash.DataLayer.upsert(changeset, changeset.context.private.upsert_keys)
                        |> set_result_tenant_metadata(changeset)
                        |> manage_relationships(changeset)

                      true ->
                        changeset.resource
                        |> Ash.DataLayer.create(changeset)
                        |> set_result_tenant_metadata(changeset)
                        |> manage_relationships(changeset)
                    end
                  else
                    {:error, changeset}
                  end
                  |> case do
                    {:ok, result, instructions} ->
                      {:ok, result, instructions}

                    {:error, error} ->
                      {:error, Ash.Changeset.add_error(changeset, error)}
                  end
              end
            else
              {:error, changeset}
            end
        end
      end,
      transaction?: changeset.action.transaction?,
      timeout: changeset.timeout,
      tracer: changeset.context.private.tracer,
      return_notifications?: changeset.context.private.return_notifications?,
      transaction_metadata: %{
        type: :create,
        metadata: %{
          resource: changeset.resource,
          action: changeset.action.name,
          actor: actor
        }
      }
    )
    |> case do
      {:ok, result, changeset, instructions} ->
        {:ok,
         %{
           result: result,
           changeset: changeset,
           notifications: instructions[:notifications]
         }}

      {:error, %Ash.Changeset{} = changeset} ->
        {:error, changeset}

      {:error, error} ->
        {:error, Ash.Changeset.add_error(changeset, error)}
    end
  end

  defp set_result_tenant_metadata({:ok, nil}, _), do: {:ok, nil}

  defp set_result_tenant_metadata({:ok, data}, changeset) do
    if changeset.tenant do
      {:ok, Ash.Resource.set_metadata(data, %{tenant: changeset.tenant})}
    else
      {:ok, data}
    end
  end

  defp set_result_tenant_metadata(other, _), do: other

  defp manage_relationships({:ok, nil}, _) do
    {:ok, nil, %{notifications: []}}
  end

  defp manage_relationships(
         {:ok, created, %{notifications: notifications}},
         changeset
       ) do
    case manage_relationships({:ok, created}, changeset) do
      {:ok, created, info} ->
        {:ok, created, Map.update(info, :notifications, notifications, &(&1 ++ notifications))}

      {:error, %Ash.Changeset{} = changeset} ->
        {:error, changeset}

      {:error, error} ->
        {:error, Ash.Changeset.add_error(changeset, error)}
    end
  end

  defp manage_relationships({:ok, created}, changeset) do
    opts =
      changeset.context.private
      |> Map.take([:actor, :authorize?, :upsert?])
      |> Map.to_list()

    with {:ok, loaded} <-
           Ash.Actions.ManagedRelationships.load(changeset.api, created, changeset, opts),
         {:ok, with_relationships, new_notifications} <-
           Ash.Actions.ManagedRelationships.manage_relationships(
             loaded,
             changeset,
             opts[:actor],
             opts
           ) do
      {:ok, with_relationships, %{notifications: new_notifications}}
    else
      {:error, %Ash.Changeset{} = changeset} ->
        {:error, changeset}

      {:error, error} ->
        {:error, Ash.Changeset.add_error(changeset, error)}
    end
  end

  defp manage_relationships(other, _), do: other
end
