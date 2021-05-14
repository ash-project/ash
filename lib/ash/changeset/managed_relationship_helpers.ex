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
        join_action = Ash.Resource.Info.primary_action!(relationship.through, :create)
        {:create, action.name, join_action.name, []}

      {:create, action_name} when relationship.type == :many_to_many ->
        join_action = Ash.Resource.Info.primary_action!(relationship.through, :create)
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

        join_action = Ash.Resource.Info.primary_action!(relationship.through, :destroy)

        {:destroy, action.name, join_action.name}

      {:destroy, action_name} when relationship.type == :many_to_many ->
        join_action = Ash.Resource.Info.primary_action!(relationship.through, :destroy)

        {:destroy, action_name, join_action.name}

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
      key when relationship.type == :many_to_many and key in [:relate, :relate_and_update] ->
        {key, primary_action_name(relationship.through, :create),
         primary_action_name(relationship.destination, :read)}

      {key, action}
      when relationship.type == :many_to_many and
             key in [:relate, :relate_and_update] ->
        {key, action, primary_action_name(relationship.destination, :read)}

      {key, action, read}
      when relationship.type == :many_to_many and
             key in [:relate, :relate_and_update] ->
        {key, action, read}

      key
      when relationship.type in [:has_many, :has_one] and key in [:relate, :relate_and_update] ->
        {key, primary_action_name(relationship.destination, :update),
         primary_action_name(relationship.destination, :read)}

      {key, update}
      when relationship.type in [:has_many, :has_one] and key in [:relate, :relate_and_update] ->
        {key, update, primary_action_name(relationship.destination, :read)}

      key when key in [:relate, :relate_and_update] ->
        {key, primary_action_name(relationship.source, :update),
         primary_action_name(relationship.destination, :read)}

      {key, update} when key in [:relate, :relate_and_update] ->
        {key, update, primary_action_name(relationship.destination, :read)}

      other ->
        other
    end)
  end

  def on_match_destination_actions(opts, relationship) do
    opts = sanitize_opts(relationship, opts)

    cond do
      opts[:on_match] in [:ignore, :error] ->
        nil

      unwrap(opts[:on_match]) == :unrelate ->
        nil

      opts[:on_match] == :no_match ->
        on_no_match_destination_actions(opts, relationship)

      opts[:on_match] == :missing ->
        on_missing_destination_actions(opts, relationship)

      unwrap(opts[:on_match]) == :update ->
        case opts[:on_match] do
          :update ->
            all(destination(primary_action_name(relationship.destination, :update)))

          {:update, action_name} ->
            all(destination(action_name))

          {:update, action_name, join_table_action_name, keys} ->
            all([destination(action_name), join(join_table_action_name, keys)])
        end
    end
  end

  def on_no_match_destination_actions(opts, relationship) do
    opts = sanitize_opts(relationship, opts)

    case opts[:on_no_match] do
      value when value in [:ignore, :error] ->
        nil

      :match ->
        on_match_destination_actions(opts, relationship)

      :create ->
        all(destination(primary_action_name(relationship.destination, :create)))

      {:create, action_name} ->
        all(destination(action_name))

      {:create, _action_name, join_table_action_name, :all} ->
        all([join(join_table_action_name, :all)])

      {:create, action_name, join_table_action_name, keys} ->
        all([destination(action_name), join(join_table_action_name, keys)])
    end
  end

  def on_missing_destination_actions(opts, relationship) do
    opts = sanitize_opts(relationship, opts)

    case opts[:on_missing] do
      :destroy ->
        all(destination(primary_action_name(relationship.destination, :destroy)))

      {:destroy, action_name} ->
        all(destination(action_name))

      {:destroy, action_name, join_resource_action_name} ->
        all([destination(action_name), join(join_resource_action_name, [])])

      _ ->
        nil
    end
  end

  def on_lookup_update_action(opts, relationship) do
    opts = sanitize_opts(relationship, opts)

    if unwrap(opts[:on_lookup]) not in [:relate, :ignore] do
      case opts[:on_lookup] do
        :relate_and_update when relationship.type == :many_to_many ->
          join(primary_action_name(relationship.through, :create), [])

        {:relate_and_update, action_name} when relationship.type == :many_to_many ->
          join(action_name, action_name)

        {:relate_and_update, action_name, _} when relationship.type == :many_to_many ->
          join(action_name, [])

        {:relate_and_update, action_name, _, keys} when relationship.type == :many_to_many ->
          join(action_name, keys)

        :relate_and_update when relationship.type in [:has_one, :has_many] ->
          destination(primary_action_name(relationship.destination, :update))

        :relate_and_update when relationship.type in [:belongs_to] ->
          source(primary_action_name(relationship.source, :update))

        {:relate_and_update, action_name} ->
          destination(action_name)

        {:relate_and_update, action_name, _} ->
          destination(action_name)
      end
    end
  end

  defp all(values) do
    case Enum.filter(List.wrap(values), & &1) do
      [] -> nil
      values -> values
    end
  end

  defp source(nil), do: nil
  defp source(action), do: {:source, action}

  defp destination(nil), do: nil
  defp destination(action), do: {:destination, action}

  defp join(nil, _), do: nil
  defp join(action_name, keys), do: {:join, action_name, keys}

  def could_handle_missing?(opts) do
    opts[:on_missing] not in [:ignore, :error]
  end

  def could_lookup?(opts) do
    opts[:on_lookup] != :ignore
  end

  def could_create?(opts) do
    unwrap(opts[:on_no_match]) == :create || unwrap(opts[:on_match]) == :no_match
  end

  def could_update?(opts) do
    unwrap(opts[:on_match]) not in [:ignore, :no_match, :missing]
  end

  def must_load?(opts) do
    only_creates_or_ignores? =
      unwrap(opts[:on_match]) in [:no_match, :ignore] &&
        unwrap(opts[:on_no_match]) in [:create, :ignore]

    can_skip_load? = opts[:on_missing] == :ignore && only_creates_or_ignores?

    not can_skip_load?
  end

  defp primary_action_name(resource, type) do
    primary_action = Ash.Resource.Info.primary_action(resource, type)

    if primary_action do
      primary_action.name
    else
      primary_action
    end
  end

  defp unwrap(value) when is_atom(value), do: value
  defp unwrap(tuple) when is_tuple(tuple), do: elem(tuple, 0)
  defp unwrap(value), do: value
end
