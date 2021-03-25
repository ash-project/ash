defmodule Ash.Changeset.ManagedRelationshipHelpers do
  @moduledoc """
  Tools for introspecting managed relationships.

  Extensions can use this to look at an argument that will be passed
  to a `manage_relationship` change and determine what their behavior
  should be. For example, AshAdmin uses these to find out what kind of
  nested form it should offer for each argument that manages a relationship.
  """

  def sanitize_opts(relationship, opts) do
    [
      on_no_match: :ignore,
      on_missing: :ignore,
      on_match: :ignore,
      on_lookup: :ignore
    ]
    |> Keyword.merge(opts)
    |> Keyword.update!(:on_no_match, fn
      :create when relationship.type == :many_to_many ->
        action = Ash.Resource.Info.primary_action!(relationship.destination, :create)
        join_action = Ash.Resource.Info.primary_action!(relationship.through_destination, :create)
        {:create, action.name, join_action.name, []}

      {:create, action_name} when relationship.type == :many_to_many ->
        join_action = Ash.Resource.Info.primary_action!(relationship.through_destination, :create)
        {:create, action_name, join_action.name, []}

      :create ->
        action = Ash.Resource.Info.primary_action!(relationship.destination, :create)
        {:create, action.name}

      other ->
        other
    end)
    |> Keyword.update!(:on_missing, fn
      :destroy when relationship.type == :many_to_many ->
        action = Ash.Resource.Info.primary_action!(relationship.destination, :destroy)

        join_action =
          Ash.Resource.Info.primary_action!(relationship.through_destination, :destroy)

        {:destroy, action.name, join_action.name, []}

      {:destroy, action_name} when relationship.type == :many_to_many ->
        join_action =
          Ash.Resource.Info.primary_action!(relationship.through_destination, :destroy)

        {:destroy, action_name, join_action.name, []}

      :destroy ->
        action = Ash.Resource.Info.primary_action!(relationship.destination, :destroy)

        {:destroy, action.name}

      :unrelate ->
        {:unrelate, nil}

      other ->
        other
    end)
    |> Keyword.update!(:on_match, fn
      :update when relationship.type == :many_to_many ->
        update = Ash.Resource.Info.primary_action!(relationship.destination, :update)
        join_update = Ash.Resource.Info.primary_action!(relationship.through, :update)

        {:update, update.name, join_update.name, []}

      {:update, update} when relationship.type == :many_to_many ->
        join_update = Ash.Resource.Info.primary_action!(relationship.through, :update)

        {:update, update, join_update.name, []}

      {:update, update, join_update} when relationship.type == :many_to_many ->
        {:update, update, join_update, []}

      :update ->
        action = Ash.Resource.Info.primary_action!(relationship.destination, :update)

        {:update, action.name}

      :unrelate ->
        {:unrelate, nil}

      other ->
        other
    end)
    |> Keyword.update!(:on_lookup, fn
      operation
      when relationship.type == :many_to_many and
             operation in [:relate, :relate_and_update] ->
        read = Ash.Resource.Info.primary_action(relationship.destination, :read)
        create = Ash.Resource.Info.primary_action(relationship.through, :create)

        {operation, create.name, read.name, []}

      operation
      when relationship.type in [:has_many, :has_one] and
             operation in [:relate, :relate_and_update] ->
        read = Ash.Resource.Info.primary_action(relationship.destination, :read)
        update = Ash.Resource.Info.primary_action(relationship.destination, :update)

        {operation, update.name, read.name}

      operation when operation in [:relate, :relate_and_update] ->
        read = Ash.Resource.Info.primary_action(relationship.destination, :read)
        update = Ash.Resource.Info.primary_action(relationship.source, :update)

        {operation, update.name, read.name}

      :ignore ->
        :ignore
    end)
  end

  def could_lookup?(opts) do
    opts[:on_lookup] != :ignore
  end

  def must_load?(opts) do
    only_creates? = unwrap(opts[:on_match]) == :create && unwrap(opts[:on_no_match]) == :create
    only_ignores? = opts[:on_no_match] == :ignore && opts[:on_match] == :ignore
    can_skip_load? = opts[:on_missing] == :ignore && (only_creates? || only_ignores?)

    not can_skip_load?
  end

  defp unwrap(value) when is_atom(value), do: true
  defp unwrap(tuple) when is_tuple(tuple), do: elem(tuple, 0)
  defp unwrap(value), do: value
end
