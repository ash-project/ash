defmodule Ash.Authorization.Check.RelatedToUserVia do
  use Ash.Authorization.Check, action_types: [:read, :update, :delete]

  @impl true
  def describe(opts) do
    description = describe_relationship(opts[:resource], opts[:relationship])

    description <> "this_record is the user"
  end

  # TODO: If they aren't filtering on the "user equaling this", but are
  # filtering based on field values and we can trace those field value
  # filters from the record all the way to the user, then we can
  # allow this at strict check time

  @impl true
  def strict_check(%user_resource{} = user, request = %{action_type: :read}, opts) do
    full_relationship_path = opts[:relationship]

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

  def strict_check(_, _, _), do: {:ok, :unknown}

  @impl true
  def prepare(opts) do
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
    record
    |> List.wrap()
    |> Enum.flat_map(fn record ->
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
