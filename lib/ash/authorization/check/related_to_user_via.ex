defmodule Ash.Authorization.Check.RelatedToUserVia do
  use Ash.Authorization.Check, action_types: [:read, :update, :delete]

  @impl true
  def describe(opts) do
    description = describe_relationship(opts[:source], opts[:relationship])

    description <> "this_record is the user"
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
  def prepare(opts) do
    [side_load: put_into_relationship_path(opts[:relationship], [])]
  end

  @impl true
  def check(user, records, _state, options) do
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

  defp describe_relationship(resource, relationships) do
    reversed_relationships =
      relationships
      |> Enum.reduce({resource, []}, fn relationship_name, {resource, acc} ->
        relationship = Ash.relationship(resource, relationship_name)
        {relationship.destination, [relationship | acc]}
      end)
      |> elem(1)

    do_describe_relationship(reversed_relationships)
  end

  defp do_describe_relationship([]), do: ""

  defp do_describe_relationship([%{name: name, cardinality: :many} | rest]) do
    "one of the #{name} of " <> do_describe_relationship(rest)
  end

  defp do_describe_relationship([%{name: name, cardinality: :one} | rest]) do
    "the #{name} of " <> do_describe_relationship(rest)
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
