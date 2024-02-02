defmodule Ash.Changeset.ManagedRelationshipHelpers do
  @moduledoc """
  Tools for introspecting managed relationships.

  Extensions can use this to look at an argument that will be passed
  to a `manage_relationship` change and determine what their behavior
  should be. For example, AshAdmin uses these to find out what kind of
  nested form it should offer for each argument that manages a relationship.
  """

  def sanitize_opts(relationship, opts) do
    is_many_to_many = relationship.type == :many_to_many
    join_keys = opts[:join_keys] || []

    [
      on_no_match: :ignore,
      on_missing: :ignore,
      on_match: :ignore,
      on_lookup: :ignore
    ]
    |> Keyword.merge(opts)
    |> Keyword.update!(:on_no_match, fn
      :create when is_many_to_many ->
        create = primary_action_name!(relationship.destination, :create)
        join_create = primary_action_name!(relationship.through, :create)
        {:create, create, join_create, join_keys}

      {:create, create} when is_many_to_many ->
        join_create = primary_action_name!(relationship.through, :create)
        {:create, create, join_create, join_keys}

      {:create, create, join_create} when is_many_to_many ->
        {:create, create, join_create, join_keys}

      :create ->
        create = primary_action_name!(relationship.destination, :create)
        {:create, create}

      other ->
        other
    end)
    |> Keyword.update!(:on_missing, fn
      :destroy when is_many_to_many ->
        destroy = primary_action_name!(relationship.destination, :destroy)
        join_destroy = primary_action_name!(relationship.through, :destroy)
        {:destroy, destroy, join_destroy}

      {:destroy, destroy} when is_many_to_many ->
        join_destroy = primary_action_name!(relationship.through, :destroy)
        {:destroy, destroy, join_destroy}

      :destroy ->
        destroy = primary_action_name!(relationship.destination, :destroy)
        {:destroy, destroy}

      :unrelate ->
        {:unrelate, nil}

      other ->
        other
    end)
    |> Keyword.update!(:on_match, fn
      :update when is_many_to_many ->
        update = primary_action_name!(relationship.destination, :update)
        join_update = primary_action_name!(relationship.through, :update)
        {:update, update, join_update, join_keys}

      {:update, update} when is_many_to_many ->
        join_update = primary_action_name!(relationship.through, :update)
        {:update, update, join_update, join_keys}

      {:update, update, join_update} when is_many_to_many ->
        {:update, update, join_update, join_keys}

      :update ->
        update = primary_action_name!(relationship.destination, :update)
        {:update, update}

      :unrelate ->
        {:unrelate, nil}

      :destroy when is_many_to_many ->
        destroy = primary_action_name!(relationship.through, :destroy)
        {:destroy, destroy}

      :destroy ->
        destroy = primary_action_name!(relationship.destination, :destroy)
        {:destroy, destroy}

      other ->
        other
    end)
    |> Keyword.update!(:on_lookup, fn
      key when is_many_to_many and key in [:relate, :relate_and_update] ->
        join_create = primary_action_name!(relationship.through, :create)
        read = primary_action_name!(relationship.destination, :read)
        {key, join_create, read, join_keys}

      {key, join_create} when is_many_to_many and key in [:relate, :relate_and_update] ->
        read = primary_action_name!(relationship.destination, :read)
        {key, join_create, read, join_keys}

      {key, join_create, read} when is_many_to_many and key in [:relate, :relate_and_update] ->
        {key, join_create, read, join_keys}

      key
      when relationship.type in [:has_many, :has_one] and key in [:relate, :relate_and_update] ->
        update = primary_action_name!(relationship.destination, :update)
        read = primary_action_name!(relationship.destination, :read)
        {key, update, read}

      key when key in [:relate, :relate_and_update] ->
        update = primary_action_name(relationship.source, :update)
        read = primary_action_name!(relationship.destination, :read)
        {key, update, read}

      {key, update} when key in [:relate, :relate_and_update] ->
        read = primary_action_name!(relationship.destination, :read)
        {key, update, read}

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

      unwrap(opts[:on_match]) == :destroy && relationship.type == :many_to_many ->
        case opts[:on_match] do
          :destroy ->
            all(join(primary_action_name(relationship.through, :destroy), :all))

          {:destroy, action_name} ->
            all(join(action_name, :all))
        end

      unwrap(opts[:on_match]) == :destroy ->
        case opts[:on_match] do
          :destroy ->
            all(destination(primary_action_name(relationship.destination, :destroy)))

          {:destroy, action_name} ->
            all(destination(action_name))
        end

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

        _ ->
          nil
      end
    end
  end

  def on_lookup_read_action(opts, relationship) do
    opts = sanitize_opts(relationship, opts)

    if unwrap(opts[:on_lookup]) not in [:ignore] do
      case opts[:on_lookup] do
        :relate ->
          destination(primary_action_name(relationship.destination, :read))

        {:relate, _} ->
          destination(primary_action_name(relationship.destination, :read))

        {:relate, _, read} ->
          destination(read)

        :relate_and_update when relationship.type in [:has_one, :has_many] ->
          destination(primary_action_name(relationship.destination, :read))

        :relate_and_update when relationship.type in [:belongs_to] ->
          source(primary_action_name(relationship.source, :read))

        {:relate_and_update, _} ->
          destination(:read)

        {:relate_and_update, _action_name, read} ->
          destination(read)

        {:relate_and_update, _action_name, read, _} ->
          destination(read)
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

  def must_load?(opts, must_load_opts \\ []) do
    creating_and_cant_have_related? =
      must_load_opts[:could_be_related_at_creation?] == false &&
        must_load_opts[:action_type] == :create

    allowed_on_no_match_values =
      if creating_and_cant_have_related? do
        [:create, :ignore, :error]
      else
        [:create, :ignore]
      end

    only_creates_or_ignores? =
      creating_and_cant_have_related? ||
        (unwrap(opts[:on_match]) in [:no_match, :ignore] &&
           unwrap(opts[:on_no_match]) in allowed_on_no_match_values)

    on_missing_can_skip_load? =
      if must_load_opts[:could_be_related_at_creation?] == false &&
           must_load_opts[:action_type] == :create do
        true
      else
        opts[:on_missing] == :ignore
      end

    can_skip_load? = on_missing_can_skip_load? && only_creates_or_ignores?

    not can_skip_load?
  end

  defp primary_action_name(resource, type) do
    if primary_action = Ash.Resource.Info.primary_action(resource, type) do
      primary_action.name
    end
  end

  defp primary_action_name!(resource, type) do
    primary_action = Ash.Resource.Info.primary_action!(resource, type)
    primary_action.name
  end

  defp unwrap(value) when is_atom(value), do: value
  defp unwrap(tuple) when is_tuple(tuple), do: elem(tuple, 0)
  defp unwrap(value), do: value
end
