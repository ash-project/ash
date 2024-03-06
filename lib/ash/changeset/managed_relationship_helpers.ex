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
      :unrelate when is_many_to_many ->
        join_destroy = primary_action_name!(relationship.through, :destroy)
        {:unrelate, join_destroy}

      :unrelate when relationship.type in [:has_many, :has_one] ->
        update = primary_action_name!(relationship.destination, :update)
        {:unrelate, update}

      :unrelate ->
        {:unrelate, nil}

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

      :update_join ->
        join_update = primary_action_name!(relationship.through, :update)
        {:update, nil, join_update, join_keys}

      {:update_join, join_update} ->
        {:update, nil, join_update, join_keys}

      {:update_join, join_update, join_keys} ->
        {:update, nil, join_update, join_keys}

      :unrelate when is_many_to_many ->
        join_destroy = primary_action_name!(relationship.through, :destroy)
        {:unrelate, join_destroy}

      :unrelate when relationship.type in [:has_many, :has_one] ->
        update = primary_action_name!(relationship.destination, :update)
        {:unrelate, update}

      :unrelate ->
        {:unrelate, nil}

      :destroy when is_many_to_many ->
        join_destroy = primary_action_name!(relationship.through, :destroy)
        {:destroy, join_destroy}

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
        read = primary_action_name!(relationship.destination, :read)
        {key, nil, read}

      {key, update} when key in [:relate, :relate_and_update] ->
        read = primary_action_name!(relationship.destination, :read)
        {key, update, read}

      other ->
        other
    end)
  end

  def on_match_destination_actions(opts, relationship) do
    opts = sanitize_opts(relationship, opts)

    case opts[:on_match] do
      key when key in [:ignore, :error] ->
        nil

      :no_match ->
        on_no_match_destination_actions(opts, relationship)

      :missing ->
        on_missing_destination_actions(opts, relationship)

      {:update, update, join_update, join_keys} ->
        all([destination(update), join(join_update, join_keys)])

      {:update, update} ->
        all(destination(update))

      {:unrelate, join_destroy} when relationship.type == :many_to_many ->
        all(join(join_destroy, :*))

      {:unrelate, update} when relationship.type in [:has_one, :has_many] ->
        all(destination(update))

      {:unrelate, _nil} ->
        nil

      {:destroy, join_destroy} when relationship.type == :many_to_many ->
        all(join(join_destroy, :*))

      {:destroy, destroy} ->
        all(destination(destroy))
    end
  end

  def on_no_match_destination_actions(opts, relationship) do
    opts = sanitize_opts(relationship, opts)

    case opts[:on_no_match] do
      key when key in [:ignore, :error] ->
        nil

      :match ->
        on_match_destination_actions(opts, relationship)

      {:create, create, join_create, join_keys} ->
        all([destination(create), join(join_create, join_keys)])

      {:create, create} ->
        all(destination(create))
    end
  end

  def on_missing_destination_actions(opts, relationship) do
    opts = sanitize_opts(relationship, opts)

    case opts[:on_missing] do
      key when key in [:ignore, :error] ->
        nil

      {:unrelate, join_destroy} when relationship.type == :many_to_many ->
        all(join(join_destroy, []))

      {:unrelate, update} when relationship.type in [:has_one, :has_many] ->
        all(destination(update))

      {:unrelate, _nil} ->
        nil

      {:destroy, destroy, join_destroy} ->
        all([destination(destroy), join(join_destroy, [])])

      {:destroy, destroy} ->
        all(destination(destroy))
    end
  end

  def on_lookup_update_action(opts, relationship) do
    opts = sanitize_opts(relationship, opts)

    case opts[:on_lookup] do
      :ignore ->
        nil

      {key, join_create, _read, join_keys} when key in [:relate, :relate_and_update] ->
        join(join_create, join_keys)

      {key, update, _read}
      when key in [:relate, :relate_and_update] and relationship.type in [:has_one, :has_many] ->
        destination(update)

      {key, _nil, _read} when key in [:relate, :relate_and_update] ->
        nil
    end
  end

  def on_lookup_read_action(opts, relationship) do
    opts = sanitize_opts(relationship, opts)

    case opts[:on_lookup] do
      :ignore ->
        nil

      {key, _join_create, read, _join_keys} when key in [:relate, :relate_and_update] ->
        destination(read)

      {key, _update, read} when key in [:relate, :relate_and_update] ->
        destination(read)
    end
  end

  defp all(values) do
    case Enum.filter(List.wrap(values), & &1) do
      [] -> nil
      values -> values
    end
  end

  defp destination(nil), do: nil
  defp destination(action), do: {:destination, action}

  defp join(nil, _), do: nil
  defp join(action, keys), do: {:join, action, keys}

  def could_handle_missing?(opts) do
    opts[:on_missing] not in [:ignore, :error]
  end

  def could_lookup?(opts) do
    opts[:on_lookup] != :ignore
  end

  def could_create?(opts) do
    unwrap(opts[:on_no_match]) == :create || opts[:on_match] == :no_match
  end

  def could_update?(opts) do
    opts[:on_match] not in [:ignore, :no_match, :missing]
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
        (opts[:on_match] in [:no_match, :ignore] &&
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

  defp primary_action_name!(resource, type) do
    Ash.Resource.Info.primary_action!(resource, type).name
  end

  defp unwrap(value) when is_atom(value), do: value
  defp unwrap(tuple) when is_tuple(tuple), do: elem(tuple, 0)
  defp unwrap(value), do: value
end
