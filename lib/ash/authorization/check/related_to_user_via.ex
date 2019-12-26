defmodule Ash.Authorization.Check.RelatedToUserVia do
  use Ash.Authorization.Check

  def related_to_user_via(relationship) do
    {__MODULE__, relationship: List.wrap(relationship)}
  end

  @impl true
  def describe(opts) do
    "#{Enum.join(opts[:relationship], ".")} is the user"
  end

  @impl true
  def strict_check(%user_resource{} = user, request, opts) do
    full_relationship_path = request.relationship ++ opts[:relationship]

    pkey_filter = user |> Map.take(Ash.primary_key(user_resource)) |> Map.to_list()

    candidate_filter = put_into_relationship_path(full_relationship_path, pkey_filter)

    case Ash.Filter.parse(request.resource, candidate_filter) do
      %{errors: []} = parsed ->
        if Ash.Filter.strict_subset_of?(parsed, request.filter) do
          {:ok, true}
        else
          {:ok, :unknown}
        end

      %{errors: errors} ->
        {:error, errors}
    end
  end

  @impl true
  def prepare(_user, _request, opts) do
    [side_load: put_into_relationship_path(opts[:relationship], [])]
  end

  @impl true
  def check(user, records, _request, options) do
    matches =
      Enum.filter(records, fn record ->
        related_records = get_related(record, options[:relationship])

        Enum.any?(related_records, fn related ->
          primary_key = Ash.primary_key(user)
          Map.take(related, primary_key) == Map.take(user, primary_key)
        end)
      end)

    {:ok, matches}
  end

  defp get_related(record, []), do: record

  defp get_related(record, [relationship | rest]) do
    Enum.flat_map(record, fn record ->
      record
      |> Map.get(relationship)
      |> List.wrap()
      |> Enum.map(&get_related(&1, rest))
    end)
  end

  defp put_into_relationship_path([], value), do: value

  defp put_into_relationship_path([item | rest], value) do
    [{item, put_into_relationship_path(rest, value)}]
  end
end
