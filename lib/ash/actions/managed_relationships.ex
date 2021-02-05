defmodule Ash.Actions.ManagedRelationships do
  @moduledoc false
  require Ash.Query

  alias Ash.Error.Changes.InvalidRelationship

  def setup_managed_belongs_to_relationships(changeset, actor) do
    changeset.relationships
    |> Enum.filter(fn {_key, value} ->
      Map.has_key?(value, :manage)
    end)
    |> Enum.map(fn {key, val} ->
      {Ash.Resource.relationship(changeset.resource, key), Map.get(val, :manage)}
    end)
    |> Enum.filter(fn {relationship, _val} ->
      relationship.type == :belongs_to
    end)
    |> Enum.reject(fn {_relationship, {input, _opts}} ->
      is_nil(input)
    end)
    |> Enum.reduce_while({changeset, %{notifications: []}}, fn {relationship, {input, opts}},
                                                               {changeset, instructions} ->
      pkeys = pkeys(relationship)

      opts = sanitize_opts(relationship, opts)
      current_value = Map.get(changeset.data, relationship.name)

      case find_match(List.wrap(current_value), input, pkeys) do
        nil ->
          case opts[:on_create] do
            :ignore ->
              {:cont, {changeset, instructions}}

            :error ->
              {:halt,
               Ash.Changeset.add_error(
                 changeset,
                 InvalidRelationship.exception(
                   relationship: relationship.name,
                   message: "Changes would create a new related record"
                 )
               )}

            {:create, action_name} ->
              create_belongs_to_record(
                relationship,
                action_name,
                input,
                changeset,
                actor,
                opts,
                instructions
              )
          end

        _value ->
          {:cont, {changeset, instructions}}
      end
    end)
  end

  defp create_belongs_to_record(
         relationship,
         action_name,
         input,
         changeset,
         actor,
         opts,
         instructions
       ) do
    relationship.destination
    |> Ash.Changeset.for_create(action_name, input)
    |> changeset.api.create(
      actor: actor,
      authorize?: opts[:authorize?],
      return_notifications?: true
    )
    |> case do
      {:ok, created, notifications} ->
        changeset =
          changeset
          |> Ash.Changeset.set_context(%{
            belongs_to_manage_created: %{relationship.name => created}
          })
          |> Ash.Changeset.force_change_attribute(
            relationship.source_field,
            Map.get(created, relationship.destination_field)
          )

        {:cont,
         {changeset, %{instructions | notifications: instructions.notifications ++ notifications}}}

      {:error, error} ->
        {:halt, Ash.Changeset.add_error(changeset, error)}
    end
  end

  def manage_relationships(record, changeset, actor) do
    changeset.relationships
    |> Enum.filter(fn {_key, value} ->
      Map.has_key?(value, :manage)
    end)
    |> Enum.map(fn {key, val} ->
      {Ash.Resource.relationship(changeset.resource, key), Map.get(val, :manage)}
    end)
    |> Enum.reduce_while({:ok, record, []}, fn {relationship, {inputs, opts}},
                                               {:ok, record, all_notifications} ->
      inputs =
        if relationship.cardinality == :many do
          List.wrap(inputs)
        else
          inputs
        end

      case manage_relationship(record, relationship, inputs, changeset, actor, opts) do
        {:ok, record, notifications} ->
          {:cont, {:ok, record, notifications ++ all_notifications}}

        {:error, error} ->
          {:halt, {:error, error}}
      end
    end)
  end

  defp sanitize_opts(relationship, opts) do
    [
      on_create: :create,
      on_destroy: :destroy,
      on_update: :update
    ]
    |> Keyword.merge(opts)
    |> Keyword.update!(:on_create, fn
      :create when relationship.type == :many_to_many ->
        action = Ash.Resource.primary_action!(relationship.destination, :create)
        join_action = Ash.Resource.primary_action!(relationship.through_destination, :create)
        {:create, action.name, join_action.name, []}

      {:create, action_name} when relationship.type == :many_to_many ->
        join_action = Ash.Resource.primary_action!(relationship.through_destination, :create)
        {:create, action_name, join_action.name, []}

      :create ->
        action = Ash.Resource.primary_action!(relationship.destination, :create)
        {:create, action.name}

      other ->
        other
    end)
    |> Keyword.update!(:on_destroy, fn
      :destroy when relationship.type == :many_to_many ->
        action = Ash.Resource.primary_action!(relationship.destination, :destroy)
        join_action = Ash.Resource.primary_action!(relationship.through_destination, :destroy)
        {:destroy, action.name, join_action.name, []}

      {:destroy, action_name} when relationship.type == :many_to_many ->
        join_action = Ash.Resource.primary_action!(relationship.through_destination, :destroy)
        {:destroy, action_name, join_action.name, []}

      :destroy ->
        action = Ash.Resource.primary_action!(relationship.destination, :destroy)

        {:destroy, action.name}

      :unrelate ->
        {:unrelate, nil}

      other ->
        other
    end)
    |> Keyword.update!(:on_update, fn
      :update ->
        action = Ash.Resource.primary_action!(relationship.destination, :update)

        {:update, action.name}

      other ->
        other
    end)
  end

  defp pkeys(relationship) do
    identities =
      relationship.destination
      |> Ash.Resource.identities()
      |> Enum.map(& &1.keys)

    [Ash.Resource.primary_key(relationship.destination) | identities]
  end

  defp manage_relationship(
         record,
         %{cardinality: :many} = relationship,
         inputs,
         changeset,
         actor,
         opts
       ) do
    inputs = List.wrap(inputs)
    opts = sanitize_opts(relationship, opts)
    pkeys = pkeys(relationship)
    original_value = List.wrap(Map.get(record, relationship.name))

    inputs
    |> Enum.reduce_while(
      {:ok, original_value, [], []},
      fn input, {:ok, current_value, all_notifications, all_used} ->
        case handle_input(
               record,
               current_value,
               relationship,
               input,
               pkeys,
               changeset,
               actor,
               opts
             ) do
          {:ok, new_value, notifications, used} ->
            {:cont, {:ok, new_value, all_notifications ++ notifications, all_used ++ used}}

          {:error, error} ->
            {:halt, {:error, error}}
        end
      end
    )
    |> case do
      {:ok, new_value, all_notifications, all_used} ->
        case delete_unused(
               record,
               original_value,
               relationship,
               new_value,
               all_used,
               changeset,
               actor,
               opts
             ) do
          {:ok, new_value, notifications} ->
            {:ok, Map.put(record, relationship.name, new_value),
             all_notifications ++ notifications}

          {:error, error} ->
            {:error, error}
        end

      {:error, error} ->
        {:error, error}
    end
  end

  defp manage_relationship(
         record,
         %{cardinality: :one} = relationship,
         inputs,
         changeset,
         actor,
         opts
       ) do
    opts = sanitize_opts(relationship, opts)

    identities =
      relationship.destination
      |> Ash.Resource.identities()
      |> Enum.map(& &1.keys)

    pkeys = [Ash.Resource.primary_key(relationship.destination) | identities]
    original_value = List.wrap(Map.get(record, relationship.name))
    inputs = List.wrap(inputs)

    inputs
    |> Enum.reduce_while(
      {:ok, original_value, [], []},
      fn input, {:ok, current_value, all_notifications, all_used} ->
        case handle_input(
               record,
               current_value,
               relationship,
               input,
               pkeys,
               changeset,
               actor,
               opts
             ) do
          {:ok, new_value, notifications, used} ->
            {:cont, {:ok, new_value, all_notifications ++ notifications, all_used ++ used}}

          {:error, error} ->
            {:halt, {:error, error}}
        end
      end
    )
    |> case do
      {:ok, new_value, all_notifications, all_used} ->
        case delete_unused(
               record,
               original_value,
               relationship,
               new_value,
               all_used,
               changeset,
               actor,
               opts
             ) do
          {:ok, new_value, notifications} ->
            {:ok, Map.put(record, relationship.name, Enum.at(List.wrap(new_value), 0)),
             all_notifications ++ notifications}

          {:error, error} ->
            {:error, error}
        end

      {:error, error} ->
        {:error, error}
    end
  end

  defp handle_input(record, current_value, relationship, input, pkeys, changeset, actor, opts) do
    match = find_match(List.wrap(current_value), input, pkeys)

    if is_nil(match) || opts[:on_update] == :create do
      case handle_create(record, current_value, relationship, input, changeset, actor, opts) do
        {:ok, current_value, notifications} ->
          {:ok, current_value, notifications, []}

        {:error, error} ->
          {:error, error}
      end
    else
      handle_update(current_value, relationship, match, input, changeset, actor, opts)
    end
  end

  defp handle_create(record, current_value, relationship, input, changeset, actor, opts) do
    api = changeset.api

    case opts[:on_create] do
      :error ->
        {:error,
         InvalidRelationship.exception(
           relationship: relationship.name,
           message: "Changes would create a new related record"
         )}

      {:create, action_name} ->
        case changeset.context[:belongs_to_manage_created][relationship.name] do
          nil ->
            relationship.destination
            |> Ash.Changeset.for_create(action_name, input)
            |> Ash.Changeset.force_change_attribute(
              relationship.destination_field,
              Map.get(record, relationship.source_field)
            )
            |> Ash.Changeset.set_context(relationship.context)
            |> api.create(
              return_notifications?: true,
              authorize?: opts[:authorize?],
              actor: actor
            )
            |> case do
              {:ok, created, notifications} ->
                {:ok, [created | current_value], notifications}

              {:error, error} ->
                {:error, error}
            end

          created ->
            {:ok, [created | current_value], []}
        end

      {:create, action_name, join_action_name, params} ->
        join_keys = params ++ Enum.map(params, &to_string/1)
        {join_params, regular_params} = Map.split(input, join_keys)

        relationship.destination
        |> Ash.Changeset.for_create(action_name, regular_params)
        |> Ash.Changeset.set_context(relationship.context)
        |> api.create(
          return_notifications?: true,
          authorize?: opts[:authorize?],
          actor: actor
        )
        |> case do
          {:ok, created, regular_notifications} ->
            join_relationship =
              Ash.Resource.relationship(relationship.source, relationship.join_relationship)

            relationship.through
            |> Ash.Changeset.for_create(join_action_name, join_params)
            |> Ash.Changeset.set_context(join_relationship.context)
            |> api.create(
              return_notifications?: true,
              authorize?: opts[:authorize?],
              actor: actor
            )
            |> case do
              {:ok, _join_row, notifications} ->
                {:ok, [created | current_value], regular_notifications ++ notifications}

              {:error, error} ->
                {:error, error}
            end

          {:error, error} ->
            {:error, error}
        end

      :ignore ->
        {:ok, current_value, []}
    end
  end

  # credo:disable-for-next-line Credo.Check.Refactor.Nesting
  defp handle_update(current_value, relationship, match, input, changeset, actor, opts) do
    api = changeset.api

    case opts[:on_update] do
      # :create case is handled when determining updates/creates

      :error ->
        {:error,
         InvalidRelationship.exception(
           relationship: relationship.name,
           message: "Changes would update a record"
         )}

      :ignore ->
        {:ok, [match | current_value], [], [match]}

      :destroy ->
        {:ok, current_value, [], []}

      {:update, action_name} ->
        match
        |> Ash.Changeset.for_update(action_name, input)
        |> Ash.Changeset.set_context(relationship.context)
        |> api.update(actor: actor, authorize?: opts[:authorize?], return_notifications?: true)
        |> case do
          {:ok, updated, update_notifications} ->
            {:ok, [updated | current_value], update_notifications, [match]}

          {:error, error} ->
            {:error, error}
        end

      {:update, action_name, join_action_name, params} ->
        join_keys = params ++ Enum.map(params, &to_string/1)
        {join_params, regular_params} = Map.split(input, join_keys)
        source_value = Map.get(match, relationship.source_field)

        match
        |> Ash.Changeset.for_update(action_name, regular_params)
        |> Ash.Changeset.set_context(relationship.context)
        |> api.update(actor: actor, authorize?: opts[:authorize?], return_notifications?: true)
        |> case do
          {:ok, updated, update_notifications} ->
            destination_value = Map.get(updated, relationship.destination_field)

            relationship.through
            |> Ash.Query.filter(ref(^relationship.source_field_on_join_table) == ^source_value)
            |> Ash.Query.filter(
              ref(^relationship.destination_field_on_join_table) == ^destination_value
            )
            |> Ash.Actions.Read.unpaginated_read(nil,
              authorize?: opts[:authorize?],
              actor: actor
            )
            |> case do
              {:ok, results} ->
                join_relationship =
                  Ash.Resource.relationship(relationship.source, relationship.join_relationship)

                Enum.reduce_while(results, {:ok, []}, fn result, {:ok, all_notifications} ->
                  result
                  |> Ash.Changeset.for_update(join_action_name, join_params)
                  |> Ash.Changeset.set_context(join_relationship.context)
                  |> api.update(
                    return_notifications?: true,
                    authorize?: opts[:authorize?],
                    actor: actor
                  )
                  # credo:disable-for-next-line Credo.Check.Refactor.Nesting
                  |> case do
                    {:ok, join_update_notifications} ->
                      {:ok, [updated | current_value],
                       update_notifications ++ join_update_notifications ++ all_notifications,
                       [match]}

                    {:error, error} ->
                      {:halt, {:error, error}}
                  end
                end)

              {:error, error} ->
                {:error, error}
            end

          {:error, error} ->
            {:error, error}
        end
    end
  end

  defp find_match(current_value, input, pkeys) do
    Enum.find(current_value, fn current_value ->
      Enum.any?(pkeys, fn pkey ->
        matches?(current_value, input, pkey)
      end)
    end)
  end

  defp matches?(current_value, input, pkey) do
    Enum.all?(pkey, fn field ->
      with {:ok, current_val} <- Map.fetch(current_value, field),
           {:ok, input_val} <- fetch_field(input, field) do
        current_val == input_val
      else
        _ ->
          false
      end
    end)
  end

  defp fetch_field(input, field) do
    case Map.fetch(input, field) do
      {:ok, value} ->
        {:ok, value}

      :error ->
        Map.fetch(input, to_string(field))
    end
  end

  # credo:disable-for-next-line Credo.Check.Refactor.CyclomaticComplexity
  defp delete_unused(
         source_record,
         original_value,
         relationship,
         current_value,
         all_used,
         changeset,
         actor,
         opts
       ) do
    api = changeset.api

    original_value
    |> List.wrap()
    |> Enum.reject(&(&1 in all_used))
    |> Enum.reduce_while(
      {:ok, current_value, []},
      fn record, {:ok, current_value, all_notifications} ->
        case opts[:on_destroy] do
          :ignore ->
            {:cont, {:ok, [record | current_value], []}}

          {:destroy, action_name, join_action_name} ->
            source_value = Map.get(source_record, relationship.source_field)
            destination_value = Map.get(record, relationship.destination_field)

            relationship.through
            |> Ash.Query.filter(ref(^relationship.source_field_on_join_table) == ^source_value)
            |> Ash.Query.filter(
              ref(^relationship.destination_field_on_join_table) == ^destination_value
            )
            |> Ash.Actions.Read.unpaginated_read(nil,
              authorize?: opts[:authorize?],
              actor: actor
            )
            |> case do
              {:ok, results} ->
                join_relationship =
                  Ash.Resource.relationship(relationship.source, relationship.join_relationship)

                Enum.reduce_while(results, {:ok, []}, fn result, {:ok, all_notifications} ->
                  result
                  |> Ash.Changeset.for_destroy(
                    join_action_name,
                    %{}
                  )
                  |> Ash.Changeset.set_context(join_relationship.context)
                  |> api.destroy(
                    return_notifications?: true,
                    authorize?: opts[:authorize?],
                    actor: actor
                  )
                  |> case do
                    {:ok, _result, join_notifications} ->
                      {:cont, {:ok, join_notifications ++ all_notifications}}

                    {:error, error} ->
                      {:halt, {:error, error}}
                  end
                end)
                |> case do
                  {:ok, notifications} ->
                    record
                    |> Ash.Changeset.for_destroy(action_name, %{})
                    |> Ash.Changeset.set_context(relationship.context)
                    |> api.destroy(
                      return_notifications?: true,
                      authorize?: opts[:authorize?],
                      actor: actor
                    )
                    # credo:disable-for-next-line Credo.Check.Refactor.Nesting
                    |> case do
                      {:ok, destroy_destination_notifications} ->
                        {:cont,
                         {:ok,
                          notifications ++
                            all_notifications ++ destroy_destination_notifications}}

                      {:error, error} ->
                        {:error, error}
                    end

                  {:error, error} ->
                    {:halt, {:error, error}}
                end

              {:error, error} ->
                {:error, error}
            end

          {:destroy, action_name} ->
            record
            |> Ash.Changeset.for_destroy(action_name, %{})
            |> Ash.Changeset.set_context(relationship.context)
            |> api.destroy(
              authorize?: opts[:authorize?],
              actor: actor,
              return_notifications?: true
            )
            |> case do
              {:ok, notifications} ->
                {:cont, {:ok, current_value, notifications ++ all_notifications}}

              {:error, error} ->
                {:halt, {:error, error}}
            end

          :error ->
            {:halt,
             {:error,
              InvalidRelationship.exception(
                relationship: relationship.name,
                message: "Changes would destroy a record"
              )}}

          {:unrelate, action_name} ->
            case unrelate_data(
                   source_record,
                   record,
                   api,
                   actor,
                   opts,
                   action_name,
                   relationship
                 ) do
              {:ok, notifications} ->
                {:cont, {:ok, current_value, notifications}}

              {:error, error} ->
                {:halt, {:error, error}}
            end
        end
      end
    )
  end

  defp unrelate_data(
         source_record,
         record,
         api,
         actor,
         opts,
         action_name,
         %{type: :many_to_many} = relationship
       ) do
    action_name = action_name || Ash.Resource.primary_action(relationship.through, :destroy).name
    source_value = Map.get(source_record, relationship.source_field)
    destination_value = Map.get(record, relationship.destination_field)

    relationship.through
    |> Ash.Query.filter(ref(^relationship.source_field_on_join_table) == ^source_value)
    |> Ash.Query.filter(ref(^relationship.destination_field_on_join_table) == ^destination_value)
    |> Map.put(:api, api)
    |> Ash.Actions.Read.unpaginated_read(nil, authorize?: opts[:authorize?], actor: actor)
    |> case do
      {:ok, results} ->
        Enum.reduce_while(results, {:ok, []}, fn result, {:ok, all_notifications} ->
          result
          |> Ash.Changeset.for_destroy(action_name, %{})
          |> Ash.Changeset.set_context(relationship.context)
          |> api.destroy(
            return_notifications?: true,
            authorize?: opts[:authorize?],
            actor: actor
          )
          |> case do
            {:ok, notifications} ->
              {:cont, {:ok, notifications ++ all_notifications}}

            {:error, error} ->
              {:halt, {:error, error}}
          end
        end)

      {:error, error} ->
        {:error, error}
    end
  end

  defp unrelate_data(
         _source_record,
         record,
         api,
         actor,
         opts,
         action_name,
         %{type: type} = relationship
       )
       when type in [:has_many, :has_one] do
    action_name =
      action_name || Ash.Resource.primary_action(relationship.destination, :update).name

    record
    |> Ash.Changeset.for_update(action_name, %{})
    |> Ash.Changeset.force_change_attribute(relationship.destination_field, nil)
    |> Ash.Changeset.set_context(relationship.context)
    |> api.update(return_notifications?: true, actor: actor, authorize?: opts[:authorize?])
    |> case do
      {:ok, _unrelated, notifications} ->
        {:ok, notifications}

      {:error, error} ->
        {:error, error}
    end
  end

  defp unrelate_data(
         source_record,
         _record,
         api,
         actor,
         opts,
         action_name,
         %{type: :belongs_to} = relationship
       ) do
    action_name = action_name || Ash.Resource.primary_action(relationship.source, :update).name

    source_record
    |> Ash.Changeset.for_update(action_name, %{})
    |> Ash.Changeset.force_change_attribute(relationship.destination_field, nil)
    |> Ash.Changeset.set_context(relationship.context)
    |> api.update(return_notifications?: true, actor: actor, authorize?: opts[:authorize?])
    |> case do
      {:ok, _unrelated, notifications} ->
        {:ok, notifications}

      {:error, error} ->
        {:error, error}
    end
  end
end
