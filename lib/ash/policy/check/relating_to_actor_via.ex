defmodule Ash.Policy.Check.RelatingToActorVia do
  @moduledoc "This check is true when the specified relationship is related to the current actor."
  use Ash.Policy.SimpleCheck

  @default_field :id

  defp configure_opts(opts) do
    opts
    |> Keyword.update!(:relationships, &List.wrap/1)
    |> Keyword.put_new(:field, @default_field)
  end

  @impl true
  def describe(opts) do
    opts = configure_opts(opts)
    path = Enum.join(opts[:relationships], ".")
    field = Keyword.get(opts, :field, [@default_field])
    target_field = Keyword.get(opts, :target_field)

    target_field_part = if target_field, do: ".#{target_field}", else: ""
    "relating this.#{path}#{target_field_part} == actor.#{field}"
  end

  @impl true
  def match?(actor, %{changeset: %Ash.Changeset{} = changeset}, opts) when not is_nil(actor) do
    opts = configure_opts(opts)
    field = Keyword.get(opts, :field)
    target_field = Keyword.get(opts, :target_field)
    relationship_path = opts[:relationships]

    unless has_attributes?(actor, field) do
      raise "Actor does not have `#{inspect(field)}` attribute(s). Set `:field` in `relating_to_actor_via`."
    end

    first_relationship = first_relationship_info(changeset.resource, relationship_path)

    remaining_path = Enum.drop(relationship_path, 1)

    {last_relationship, to_many?} =
      relationship_info(changeset.resource, relationship_path)

    if to_many? do
      raise "Can only use `belongs_to` and `has_one` relationships in `relating_to_actor_via` checks"
    end

    pkey = Ash.Resource.Info.primary_key(last_relationship.destination)
    effective_target_field = target_field || pkey

    unless has_attributes?(last_relationship.destination, effective_target_field) do
      raise "Last resource in path does not have `#{inspect(effective_target_field)}` attribute(s). Set `:target_field` in `relating_to_actor_via`."
    end

    # get the key field to load by
    key_value = Ash.Changeset.get_attribute(changeset, first_relationship.source_attribute)
    id = Map.put(%{}, first_relationship.destination_attribute, key_value)

    # load the first resource from the database, then load the remaining path
    # grabbing the last item in the path
    last_item =
      first_relationship.destination
      |> Ash.get!(id, actor: actor)
      |> get_last_item(remaining_path, actor)

    target_values = extract_key_values(last_item, effective_target_field)
    actor_values = extract_key_values(actor, field)

    # make sure it lines up with the actor
    {:ok, target_values == actor_values}
  end

  def match?(_, _, _), do: false

  defp has_attributes?(resource, keys) when is_list(keys) do
    attributes = Ash.Resource.Info.attributes(resource)

    num_keys =
      Enum.reduce(attributes, 0, fn
        %Ash.Resource.Attribute{name: name}, acc ->
          if name in keys do
            acc + 1
          else
            acc
          end
      end)

    num_keys == length(keys)
  end

  defp has_attributes?(resource, key), do: has_attributes?(resource, [key])

  defp extract_key_values(record, keys) when is_list(keys) do
    Enum.reduce(keys, [], fn key, acc -> [Map.get(record, key) | acc] end)
  end

  defp extract_key_values(record, key), do: extract_key_values(record, [key])

  # no relationships, so we're done
  defp get_last_item(item, [], _actor), do: item

  defp get_last_item(item, relationship_path, actor) do
    load_path = relationship_path_to_load_path(relationship_path)

    relationship_path = Enum.map(relationship_path, &Access.key!/1)

    item
    |> Ash.load!(load_path, actor: actor)
    |> get_in(relationship_path)
  end

  defp relationship_path_to_load_path(relationship_path) do
    relationship_path
    |> Enum.reverse()
    |> Enum.reduce([], fn
      item_key, [] -> [item_key]
      item_key, acc -> Keyword.put([], item_key, acc)
    end)
  end

  defp first_relationship_info(resource, path, to_many? \\ false) do
    first_path =
      path
      |> List.first()
      |> List.wrap()

    {rel, _} = relationship_info(resource, first_path, to_many?)
    rel
  end

  defp relationship_info(resource, path, to_many? \\ false)

  defp relationship_info(resource, [rel_key], to_many?) do
    rel = Ash.Resource.Info.relationship(resource, rel_key)

    raise_if_nil(rel, rel_key, resource)

    {rel, to_many? || rel.cardinality == :many}
  end

  defp relationship_info(resource, [rel_key | rest], to_many?) do
    rel = Ash.Resource.Info.relationship(resource, rel_key)

    raise_if_nil(rel, rel_key, resource)

    relationship_info(rel.destination, rest, to_many? || rel.cardinality == :many)
  end

  defp raise_if_nil(nil, rel_key, resource) do
    raise "No such relationship ':#{rel_key}' for #{resource}, required in `relating_to_actor_via` check"
  end

  defp raise_if_nil(_, _, _) do
    :ok
  end
end
