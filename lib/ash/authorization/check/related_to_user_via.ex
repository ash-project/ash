defmodule Ash.Authorization.Check.RelatedToUserVia do
  use Ash.Authorization.Check

  def related_to_user_via(relationship) do
    {__MODULE__, relationship: List.wrap(relationship)}
  end

  @impl true
  def describe(relationship) do
    "#{Enum.join(relationship, ".")} is the user"
  end

  @impl true
  def strict_check(%user_resource{} = user, request, opts) do
    full_relationship_path = request.relationship ++ opts[:relationship]

    pkey_filter = user |> Map.take(Ash.primary_key(user_resource)) |> Map.to_list()

    candidate_filter = put_into_relationship_path(full_relationship_path, pkey_filter)

    case Ash.Filter.parse(request.resource, candidate_filter) do
      %{errors: []} = parsed ->
        cond do
          Ash.Filter.contains?(parsed, request.filter) ->
            [decision: true]

          request.strict_access? ->
            [decision: false]

          true ->
            []
        end

      %{errors: errors} ->
        [error: errors]
    end
  end

  defp put_into_relationship_path([], value), do: value

  defp put_into_relationship_path([item | rest], value) do
    [{item, put_into_relationship_path(rest, value)}]
  end
end
