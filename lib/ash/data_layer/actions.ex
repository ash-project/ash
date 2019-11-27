defmodule Ash.DataLayer.Actions do
  # def run_create_action(resource, action, attributes, relationships, params) do
  #   case Ash.Data.create(resource, action, attributes, relationships, params) do
  #     {:ok, record} ->
  #       Ash.Data.side_load(record, Map.get(params, :side_load, []), resource)

  #     {:error, error} ->
  #       {:error, error}
  #   end
  # end

  # def run_update_action(%resource{} = record, action, attributes, relationships, params) do
  #   with {:ok, record} <- Ash.Data.update(record, action, attributes, relationships, params),
  #        {:ok, [record]} <-
  #          Ash.Data.side_load([record], Map.get(params, :side_load, []), resource) do
  #     {:ok, record}
  #   else
  #     {:error, error} -> {:error, error}
  #   end
  # end

  # def run_destroy_action(record, action, params) do
  #   Ash.Data.delete(record, action, params)
  # end

  def run_read_action(resource, action, params) do
    auth_context = %{
      resource: resource,
      action: action,
      params: params
    }

    user = Map.get(params, :user)
    auth? = Map.get(params, :authorize?, false)

    with {%{prediction: prediction} = instructions, per_check_data}
         when prediction != :unauthorized <-
           maybe_authorize_precheck(auth?, user, action.rules, auth_context),
         query <- Ash.Data.resource_to_query(resource),
         {:ok, filtered_query} <- Ash.Data.filter(resource, query, params),
         {:ok, paginator} <-
           Ash.DataLayer.Paginator.paginate(resource, action, filtered_query, params),
         {:ok, found} <- Ash.Data.run_query(paginator.query, resource),
         {:ok, side_loaded_for_auth} <-
           Ash.DataLayer.SideLoader.side_load(
             resource,
             found,
             Map.get(instructions, :side_load, []),
             Map.take(params, [:authorize?, :user])
           ),
         :allow <-
           maybe_authorize(
             auth?,
             user,
             side_loaded_for_auth,
             action.rules,
             auth_context,
             per_check_data
           ),
         {:ok, side_loaded} <-
           Ash.DataLayer.SideLoader.side_load(
             resource,
             side_loaded_for_auth,
             Map.get(params, :side_load, []),
             Map.take(params, [:authorize?, :user])
           ) do
      {:ok, %{paginator | results: side_loaded}}
    else
      {:error, error} ->
        {:error, error}

      {%{prediction: :unauthorized}, _} ->
        # TODO: Nice errors here!
        {:error, :unauthorized}

      {:unauthorized, _data} ->
        # TODO: Nice errors here!
        {:error, :unauthorized}
    end
  end

  defp maybe_authorize(false, _, _, _, _, _), do: :allow

  defp maybe_authorize(true, user, data, rules, auth_context, per_check_data) do
    Ash.Authorization.Authorizer.authorize(user, data, rules, auth_context, per_check_data)
  end

  defp maybe_authorize_precheck(false, _, _, _), do: {%{prediction: :allow}, []}

  defp maybe_authorize_precheck(true, user, rules, auth_context) do
    Ash.Authorization.Authorizer.authorize_precheck(user, rules, auth_context)
  end
end
