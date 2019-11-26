defmodule Ash.DataLayer.Actions do
  def run_create_action(resource, action, attributes, relationships, params) do
    case Ash.Data.create(resource, action, attributes, relationships, params) do
      {:ok, record} ->
        Ash.Data.side_load(record, Map.get(params, :side_load, []), resource)

      {:error, error} ->
        {:error, error}
    end
  end

  def run_update_action(%resource{} = record, action, attributes, relationships, params) do
    with {:ok, record} <- Ash.Data.update(record, action, attributes, relationships, params),
         {:ok, [record]} <-
           Ash.Data.side_load([record], Map.get(params, :side_load, []), resource) do
      {:ok, record}
    else
      {:error, error} -> {:error, error}
    end
  end

  def run_destroy_action(record, action, params) do
    Ash.Data.delete(record, action, params)
  end

  def run_read_action(resource, action, params, auth? \\ true) do
    auth_context = %{
      resource: resource,
      action: action,
      params: params
    }

    user = Map.get(params || %{}, :user)

    with {%{prediction: prediction} = instructions, per_check_data}
         when prediction != :unauthorized <-
           maybe_authorize_precheck(auth?, user, action.rules, auth_context),
         {:ok, query} <- Ash.Data.resource_to_query(resource),
         {:ok, filtered_query} <- Ash.Data.filter(resource, query, params),
         {:ok, paginator} <-
           Ash.DataLayer.Paginator.paginate(resource, action, filtered_query, params),
         {:ok, found} <- Ash.Data.get_many(paginator.query, resource),
         side_load_param <-
           deep_merge_side_loads(
             Map.get(params, :side_load, []),
             Map.get(instructions, :side_load, [])
           ),
         {:ok, side_loaded} <-
           Ash.Data.side_load(found, side_load_param, resource),
         :allow <-
           maybe_authorize(auth?, user, side_loaded, action.rules, auth_context, per_check_data) do
      {:ok, %{paginator | results: side_loaded}}
    else
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

  defp deep_merge_side_loads(left, right) do
    left_sanitized = sanitize_side_load_part(left)
    right_sanitized = sanitize_side_load_part(right)

    Keyword.merge(left_sanitized, right_sanitized, fn _, v1, v2 ->
      deep_merge_side_loads(v1, v2)
    end)
  end

  defp sanitize_side_load_part(list) when is_list(list) do
    Enum.map(list, fn item ->
      case item do
        item when is_atom(item) ->
          {item, []}

        {k, v} ->
          {k, v}
      end
    end)
  end

  defp sanitize_side_load_part(item), do: [{item, []}]
end
