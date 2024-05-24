defmodule Ash.Actions.ManagedRelationships do
  @moduledoc false
  # This shouldn't be its own flow, it should use regular lifecycle hooks

  alias Ash.Error.Changes.InvalidRelationship
  alias Ash.Error.Query.NotFound

  require Ash.Query
  import Ash.Expr

  def load(_domain, created, %{relationships: rels}, _) when rels == %{},
    do: {:ok, created}

  def load(_domain, created, %{relationships: nil}, _), do: {:ok, created}

  def load(domain, created, changeset, engine_opts) do
    Enum.reduce_while(changeset.relationships, {:ok, created}, fn {key, value}, {:ok, acc} ->
      relationship = Ash.Resource.Info.relationship(changeset.resource, key)

      must_load_opts = [
        type: relationship.type,
        action_type: changeset.action.type,
        could_be_related_at_creation?:
          engine_opts[:upsert?] || Map.get(relationship, :could_be_related_at_creation?, false)
      ]

      case Enum.filter(value, fn {_, opts} ->
             opts = Ash.Changeset.ManagedRelationshipHelpers.sanitize_opts(relationship, opts)

             Ash.Changeset.ManagedRelationshipHelpers.must_load?(
               opts,
               must_load_opts
             )
           end) do
        [] ->
          {:cont, {:ok, acc}}

        relationships ->
          authorize? =
            engine_opts[:authorize?] &&
              Enum.any?(relationships, fn {_, opts} -> opts[:authorize?] end)

          actor = engine_opts[:actor]

          case Ash.load(acc, key, authorize?: authorize?, actor: actor, domain: domain) do
            {:ok, loaded} -> {:cont, {:ok, loaded}}
            {:error, error} -> {:halt, {:error, error}}
          end
      end
    end)
  end

  def setup_managed_belongs_to_relationships(changeset, actor, engine_opts) do
    changeset.relationships
    |> Enum.map(fn {relationship, val} ->
      {Ash.Resource.Info.relationship(changeset.resource, relationship), val}
    end)
    |> Enum.filter(fn {relationship, _val} ->
      relationship.type == :belongs_to
    end)
    |> Enum.flat_map(fn {relationship, inputs} ->
      inputs
      |> Enum.reject(fn {_, opts} -> opts[:ignore?] || opts[:handled?] end)
      |> Enum.with_index()
      |> Enum.map(fn {{input, opts}, index} ->
        {{relationship, {input, opts}}, index}
      end)
    end)
    |> Enum.map(fn
      {{relationship, {[input], opts}}, index} ->
        {{relationship, {input, opts}}, index}

      {{relationship, {other, opts}}, index} ->
        {{relationship, {other, opts}}, index}
    end)
    |> Enum.sort_by(fn {{_rel, {_input, opts}}, _index} ->
      opts[:meta][:order]
    end)
    |> Enum.reduce_while({changeset, %{notifications: []}}, fn
      {{relationship, {input, opts}}, index}, {changeset, instructions} ->
        opts = Ash.Changeset.ManagedRelationshipHelpers.sanitize_opts(relationship, opts)
        opts = Keyword.put(opts, :authorize?, !!(engine_opts[:authorize?] && opts[:authorize?]))
        pkeys = pkeys(relationship, opts)

        changeset =
          if changeset.action.type == :update do
            case Ash.load(changeset.data, relationship.name,
                   domain: changeset.domain,
                   authorize?: opts[:authorize?],
                   actor: actor,
                   tenant: engine_opts[:tenant]
                 ) do
              {:ok, result} ->
                {:ok, %{changeset | data: result}}

              {:error, error} ->
                {:error, error}
            end
          else
            {:ok, changeset}
          end

        case changeset do
          {:ok, changeset} ->
            changeset =
              if input in [nil, []] && opts[:on_missing] != :ignore do
                changeset
                |> maybe_force_change_attribute(relationship, :source_attribute, nil)
                |> Ash.Changeset.after_action(
                  fn _changeset, result ->
                    {:ok, Map.put(result, relationship.name, nil)}
                  end,
                  prepend?: true
                )
              else
                changeset
              end

            current_value =
              case Map.get(changeset.data, relationship.name) do
                %Ash.NotLoaded{} ->
                  nil

                other ->
                  other
              end

            input =
              if is_list(input) do
                Enum.at(input, 0)
              else
                input
              end

            match =
              if input do
                find_match(
                  List.wrap(current_value),
                  input,
                  pkeys,
                  relationship,
                  opts[:on_no_match] == :match
                )
              else
                nil
              end

            case match do
              nil ->
                case opts[:on_lookup] do
                  _ when is_nil(input) ->
                    create_belongs_to_record(
                      changeset,
                      instructions,
                      relationship,
                      input,
                      actor,
                      index,
                      opts
                    )

                  :ignore ->
                    create_belongs_to_record(
                      changeset,
                      instructions,
                      relationship,
                      input,
                      actor,
                      index,
                      opts
                    )

                  {_key, _create_or_update, read} ->
                    if is_struct(input, relationship.destination) do
                      changeset =
                        changeset
                        |> Ash.Changeset.set_context(%{
                          private: %{
                            belongs_to_manage_found: %{relationship.name => %{index => input}}
                          }
                        })
                        |> maybe_force_change_attribute(
                          relationship,
                          :source_attribute,
                          Map.get(input, relationship.destination_attribute)
                        )

                      {:cont, {changeset, instructions}}
                    else
                      Enum.find_value(pkeys(relationship, opts), fn fields ->
                        if Enum.all?(fields, fn field ->
                             has_input_value?(input, field)
                           end) do
                          Map.new(fields, &{&1, get_input_value(input, &1)})
                        end
                      end)
                      |> case do
                        nil ->
                          create_belongs_to_record(
                            changeset,
                            instructions,
                            relationship,
                            input,
                            actor,
                            index,
                            opts
                          )

                        keys ->
                          relationship.destination
                          |> Ash.Query.set_context(%{
                            accessing_from: %{
                              source: relationship.source,
                              name: relationship.name
                            }
                          })
                          |> Ash.Query.for_read(read, %{},
                            actor: actor,
                            authorize?: opts[:authorize?],
                            domain: domain(changeset, relationship)
                          )
                          |> Ash.Query.filter(^keys)
                          |> Ash.Query.do_filter(relationship.filter,
                            parent_stack: relationship.source
                          )
                          |> Ash.Query.sort(relationship.sort, prepend?: true)
                          |> Ash.Query.set_context(relationship.context)
                          |> Ash.Query.set_tenant(changeset.tenant)
                          |> Ash.read_one()
                          |> case do
                            {:ok, nil} ->
                              create_belongs_to_record(
                                changeset,
                                instructions,
                                relationship,
                                input,
                                actor,
                                index,
                                opts
                              )

                            {:ok, found} ->
                              changeset =
                                changeset
                                |> Ash.Changeset.set_context(%{
                                  private: %{
                                    belongs_to_manage_found: %{
                                      relationship.name => %{index => found}
                                    }
                                  }
                                })
                                |> maybe_force_change_attribute(
                                  relationship,
                                  :source_attribute,
                                  Map.get(found, relationship.destination_attribute)
                                )

                              {:cont, {changeset, instructions}}

                            {:error, error} ->
                              {:halt,
                               {Ash.Changeset.add_error(changeset, error, [
                                  opts[:meta][:id] || relationship.name
                                ]), instructions}}
                          end
                      end
                    end
                end

              _value ->
                if opts[:on_match] == :destroy do
                  changeset =
                    maybe_force_change_attribute(
                      changeset,
                      relationship,
                      :source_attribute,
                      nil
                    )

                  {:cont, {changeset, instructions}}
                else
                  {:cont, {changeset, instructions}}
                end
            end

          {:error, error} ->
            {:halt, {:error, error}}
        end
    end)
    |> validate_required_belongs_to(false)
    |> case do
      {:error, error} ->
        {:error, error}

      {changeset, instructions} ->
        changeset =
          Map.update!(changeset, :relationships, fn relationships ->
            Map.new(relationships, fn {rel, inputs} ->
              {rel,
               Enum.map(inputs, fn {input, config} ->
                 {input, Keyword.put(config, :handled?, true)}
               end)}
            end)
          end)

        {changeset, instructions}
    end
  end

  defp has_input_value?(value, key) when is_map(value) do
    Map.has_key?(value, key) || Map.has_key?(value, to_string(key))
  end

  defp has_input_value?(_, _), do: false

  defp get_input_value(value, key) when is_map(value) do
    case Map.fetch(value, key) do
      {:ok, value} ->
        value

      :error ->
        Map.get(value, to_string(key))
    end
  end

  defp get_input_value(_, _), do: nil

  defp domain(changeset, relationship) do
    Ash.Domain.Info.related_domain(changeset, relationship, changeset.domain)
  end

  defp maybe_force_change_attribute(changeset, %{no_attributes?: true}, _, _), do: changeset

  defp maybe_force_change_attribute(changeset, relationship, key, value) do
    Ash.Changeset.force_change_attribute(
      changeset,
      Map.get(relationship, key),
      value
    )
  end

  def validate_required_belongs_to(changeset_instructions_or_error, preflight? \\ true)
  def validate_required_belongs_to({:error, error}, _), do: {:error, error}

  def validate_required_belongs_to({%{valid?: false} = changeset, instructions}, _) do
    {changeset, instructions}
  end

  def validate_required_belongs_to({changeset, instructions}, preflight?) do
    changeset.resource
    |> Ash.Resource.Info.required_belongs_to_relationships()
    |> Enum.reduce(
      {changeset, instructions},
      fn
        required_relationship, {changeset, instructions} ->
          changeset =
            if (preflight? || changeset.relationships[required_relationship.name]) &&
                 !changeset.context[:private][:error][required_relationship.name] do
              case Ash.Changeset.get_attribute(changeset, required_relationship.source_attribute) do
                nil ->
                  Ash.Changeset.add_error(
                    changeset,
                    Ash.Error.Changes.Required.exception(
                      field: required_relationship.name,
                      type: :relationship
                    )
                  )

                _ ->
                  changeset
              end
            else
              changeset
            end

          {changeset, instructions}
      end
    )
  end

  defp create_belongs_to_record(
         changeset,
         instructions,
         relationship,
         input,
         actor,
         index,
         opts
       ) do
    if input in [nil, []] do
      {:cont, {changeset, instructions}}
    else
      case opts[:on_no_match] do
        ignore when ignore in [:ignore, :match] ->
          {:cont, {changeset, instructions}}

        :error ->
          if opts[:on_lookup] != :ignore do
            changeset =
              changeset
              |> Ash.Changeset.add_error(
                NotFound.exception(
                  primary_key: input,
                  resource: relationship.destination
                ),
                [opts[:meta][:id] || relationship.name]
              )
              |> Ash.Changeset.put_context(:private, %{
                error: %{relationship.name => true}
              })

            {:halt, {changeset, instructions}}
          else
            changeset =
              changeset
              |> Ash.Changeset.add_error(
                InvalidRelationship.exception(
                  relationship: relationship.name,
                  message: "changes would create a new related record"
                )
              )
              |> Ash.Changeset.put_context(:private, %{
                error: %{relationship.name => true}
              })

            {:halt, {changeset, instructions}}
          end

        {:create, action_name} ->
          do_create_belongs_to_record(
            relationship,
            action_name,
            input,
            changeset,
            actor,
            opts,
            instructions,
            index
          )
      end
    end
  end

  defp do_create_belongs_to_record(
         relationship,
         action_name,
         input,
         changeset,
         actor,
         opts,
         instructions,
         index
       ) do
    input =
      Map.take(
        input,
        Enum.to_list(Ash.Resource.Info.action_inputs(relationship.destination, action_name))
      )

    relationship.destination
    |> Ash.Changeset.new()
    |> Ash.Changeset.set_context(%{
      accessing_from: %{source: relationship.source, name: relationship.name}
    })
    |> Ash.Changeset.for_create(action_name, input,
      require?: false,
      actor: actor,
      tenant: changeset.tenant,
      authorize?: opts[:authorize?],
      skip_unknown_inputs: Map.keys(input),
      domain: domain(changeset, relationship)
    )
    |> Ash.Changeset.set_context(relationship.context)
    |> Ash.create(return_notifications?: true)
    |> case do
      {:ok, created, notifications} ->
        changeset =
          changeset
          |> Ash.Changeset.set_context(%{
            private: %{
              belongs_to_manage_created: %{relationship.name => %{index => created}}
            }
          })
          |> maybe_force_change_attribute(
            relationship,
            :source_attribute,
            Map.get(created, relationship.destination_attribute)
          )

        {:cont,
         {changeset, %{instructions | notifications: instructions.notifications ++ notifications}}}

      {:error, error} ->
        {:halt,
         {Ash.Changeset.add_error(changeset, error, [opts[:meta][:id] || relationship.name]),
          instructions}}
    end
  end

  def manage_relationships(record, changeset, actor, engine_opts) do
    changeset.relationships
    |> Enum.map(fn {relationship, val} ->
      {Ash.Resource.Info.relationship(changeset.resource, relationship), val}
    end)
    |> Enum.flat_map(fn {key, batches} ->
      batches
      |> Enum.reject(fn {_, opts} -> opts[:ignore?] end)
      |> Enum.with_index()
      |> Enum.map(fn {{batch, opts}, index} ->
        opts = Keyword.put(opts, :authorize?, !!(engine_opts[:authorize?] && opts[:authorize?]))
        {key, batch, opts, index}
      end)
    end)
    |> Enum.sort_by(fn {_key, _batch, opts, _index} ->
      opts[:meta][:order]
    end)
    |> Enum.reduce_while({:ok, record, []}, fn {relationship, inputs, opts, index},
                                               {:ok, record, all_notifications} ->
      inputs =
        if relationship.cardinality == :many do
          List.wrap(inputs)
        else
          inputs
        end

      case manage_relationship(record, relationship, inputs, changeset, actor, index, opts) do
        {:ok, record, notifications} ->
          record =
            if relationship.type == :many_to_many do
              Map.put(
                record,
                relationship.join_relationship,
                Map.get(record.__struct__.__struct__, relationship.join_relationship)
              )
            else
              record
            end

          {:cont, {:ok, record, notifications ++ all_notifications}}

        {:error, error} ->
          {:halt, {:error, error}}
      end
    end)
  end

  def pkeys(relationship, opts) do
    use_identities =
      case opts[:use_identities] do
        nil ->
          [:_primary_key]

        use_identities ->
          use_identities
      end

    use_identities =
      if opts[:identity_priority] do
        Enum.sort_by(use_identities, fn identity ->
          Enum.find_index(opts[:identity_priority], &(&1 == identity))
        end)
      else
        use_identities
      end

    use_identities
    |> Enum.map(fn
      :_primary_key ->
        Ash.Resource.Info.primary_key(relationship.destination)

      identity ->
        identity = Ash.Resource.Info.identity(relationship.destination, identity)

        if is_nil(identity.where) do
          identity.keys
        else
          raise ArgumentError,
                "Cannot currently use identities with a `where` statement in managed_relationships. Got #{inspect(relationship.destination)}.#{identity.name}"
        end
    end)
  end

  defp manage_relationship(
         record,
         %{cardinality: :many} = relationship,
         inputs,
         changeset,
         actor,
         index,
         opts
       ) do
    inputs = List.wrap(inputs)
    opts = Ash.Changeset.ManagedRelationshipHelpers.sanitize_opts(relationship, opts)
    pkeys = pkeys(relationship, opts)

    original_value =
      case Map.get(record, relationship.name) do
        %Ash.NotLoaded{} -> []
        value -> value
      end

    inputs
    |> Stream.with_index()
    |> Enum.reduce_while(
      {:ok, [], [], []},
      fn {input, input_index}, {:ok, current_value, all_notifications, all_used} ->
        try do
          case handle_input(
                 record,
                 current_value,
                 original_value,
                 relationship,
                 input,
                 pkeys,
                 changeset,
                 actor,
                 index,
                 opts
               ) do
            {:ok, new_value, notifications, used} ->
              {:cont, {:ok, new_value, all_notifications ++ notifications, all_used ++ used}}

            {:error, %Ash.Error.Changes.InvalidRelationship{} = error} ->
              {:halt, {:error, error}}

            {:error, error} ->
              {:halt, {:error, set_error_path(error, relationship, input_index, opts)}}
          end
        catch
          {DBConnection, ref, error} ->
            throw({DBConnection, ref, set_error_path(error, relationship, input_index, opts)})
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
            new_value =
              if is_list(new_value) do
                Enum.reverse(new_value)
              else
                new_value
              end

            {:ok, Map.put(record, relationship.name, new_value),
             all_notifications ++ notifications}

          {:error, %Ash.Error.Changes.InvalidRelationship{} = error} ->
            {:error, error}

          {:error, error} ->
            {:error, set_error_path(error, relationship, 0, opts)}
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
         index,
         opts
       ) do
    opts = Ash.Changeset.ManagedRelationshipHelpers.sanitize_opts(relationship, opts)

    identities =
      relationship.destination
      |> Ash.Resource.Info.identities()
      |> Enum.map(& &1.keys)

    pkeys = [Ash.Resource.Info.primary_key(relationship.destination) | identities]

    original_value =
      if relationship.type == :belongs_to do
        Map.get(changeset.data, relationship.name)
      else
        Map.get(record, relationship.name)
      end

    original_value =
      case original_value do
        %Ash.NotLoaded{} -> []
        value -> value
      end

    inputs = List.wrap(inputs)

    inputs
    |> Enum.reduce_while(
      {:ok, original_value, [], []},
      fn input, {:ok, current_value, all_notifications, all_used} ->
        case handle_input(
               record,
               current_value,
               original_value,
               relationship,
               input,
               pkeys,
               changeset,
               actor,
               index,
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

          {:error, %Ash.Error.Changes.InvalidRelationship{} = error} ->
            {:error, error}

          {:error, error} ->
            {:error,
             Ash.Error.set_path(
               error,
               opts[:error_path] || [opts[:meta][:id] || relationship.name]
             )}
        end

      {:error, %Ash.Error.Changes.InvalidRelationship{} = error} ->
        {:error, error}

      {:error, error} ->
        {:error, set_error_path(error, relationship, 0, opts)}
    end
  catch
    {DBConnection, ref, error} ->
      throw({DBConnection, ref, set_error_path(error, relationship, 0, opts)})
  end

  defp set_error_path(error, relationship, input_index, opts) do
    case Keyword.fetch(opts[:meta] || [], :inputs_was_list?) do
      {:ok, false} ->
        Ash.Error.set_path(
          error,
          opts[:error_path] ||
            [
              opts[:meta][:id] || relationship.name
            ]
        )

      _ ->
        Ash.Error.set_path(
          error,
          opts[:error_path] || [opts[:meta][:id] || relationship.name, input_index]
        )
    end
  end

  defp handle_input(
         record,
         current_value,
         original_value,
         relationship,
         input,
         pkeys,
         changeset,
         actor,
         index,
         opts
       ) do
    match =
      find_match(
        List.wrap(original_value),
        input,
        pkeys,
        relationship,
        opts[:on_no_match] == :match
      )

    if is_nil(match) || opts[:on_match] == :no_match do
      case handle_create(
             record,
             current_value,
             relationship,
             input,
             changeset,
             actor,
             index,
             opts
           ) do
        {:ok, current_value, notifications, used} ->
          {:ok, current_value, notifications, used}

        {:error, error} ->
          {:error, error}
      end
    else
      handle_update(record, current_value, relationship, match, input, changeset, actor, opts)
    end
  end

  defp handle_create(record, current_value, relationship, input, changeset, actor, index, opts) do
    case opts[:on_lookup] do
      :ignore ->
        do_handle_create(
          record,
          current_value,
          relationship,
          input,
          changeset,
          actor,
          index,
          opts
        )

      other ->
        case Map.fetch(
               changeset.context[:private][:belongs_to_manage_found][relationship.name] || %{},
               index
             ) do
          :error ->
            {key, create_or_update, read, join_keys} =
              case other do
                {key, create_or_update, read} -> {key, create_or_update, read, []}
                {key, create_or_update, read, keys} -> {key, create_or_update, read, keys}
              end

            case Ash.Filter.get_filter(relationship.destination, input) do
              {:ok, keys} ->
                if is_struct(input, relationship.destination) do
                  {:ok, input}
                else
                  relationship.destination
                  |> Ash.Query.set_context(%{
                    accessing_from: %{source: relationship.source, name: relationship.name}
                  })
                  |> Ash.Query.for_read(read, %{},
                    actor: actor,
                    authorize?: opts[:authorize?],
                    domain: domain(changeset, relationship)
                  )
                  |> Ash.Query.filter(^keys)
                  |> sort_and_filter(relationship)
                  |> Ash.Query.set_context(relationship.context)
                  |> Ash.Query.set_tenant(changeset.tenant)
                  |> Ash.Query.limit(1)
                  |> Ash.read_one()
                end
                |> case do
                  {:ok, found} when not is_nil(found) ->
                    do_handle_found(
                      relationship,
                      join_keys,
                      input,
                      changeset.domain,
                      opts,
                      found,
                      current_value,
                      create_or_update,
                      actor,
                      key,
                      record,
                      changeset
                    )

                  {:ok, _} ->
                    do_handle_create(
                      record,
                      current_value,
                      relationship,
                      input,
                      changeset,
                      actor,
                      index,
                      opts
                    )

                  {:error, error} ->
                    {:error, error}
                end

              {:error, _error} ->
                do_handle_create(
                  record,
                  current_value,
                  relationship,
                  input,
                  changeset,
                  actor,
                  index,
                  opts
                )
            end

          {:ok, found} ->
            {:ok, [found | current_value], [], [found]}
        end
    end
  end

  defp sort_and_filter(query, relationship) do
    # We cannot use relationship filters that reference the `parent`
    # because the parent is not yet related.
    if Ash.Actions.Read.Relationships.do_has_parent_expr?(relationship.filter) do
      query
    else
      Ash.Query.do_filter(query, relationship.filter, parent_stack: relationship.source)
    end
  end

  defp do_handle_found(
         relationship,
         join_keys,
         input,
         _domain,
         opts,
         found,
         current_value,
         create_or_update,
         actor,
         key,
         record,
         changeset
       ) do
    case relationship.type do
      :many_to_many ->
        input =
          if is_map(input) do
            input
          else
            Enum.into(input, %{})
          end

        {join_input, input} = split_join_keys(input, join_keys)

        join_relationship =
          Ash.Resource.Info.relationship(
            relationship.source,
            relationship.join_relationship
          )

        relationship.through
        |> Ash.Changeset.new()
        |> Ash.Changeset.set_context(%{
          accessing_from: %{source: join_relationship.source, name: join_relationship.name}
        })
        |> Ash.Changeset.for_create(create_or_update, join_input,
          actor: actor,
          tenant: changeset.tenant,
          authorize?: opts[:authorize?],
          require?: false,
          skip_unknown_inputs: Map.keys(join_input),
          domain: domain(changeset, join_relationship)
        )
        |> maybe_force_change_attribute(
          relationship,
          :source_attribute_on_join_resource,
          Map.get(record, relationship.source_attribute)
        )
        |> maybe_force_change_attribute(
          relationship,
          :destination_attribute_on_join_resource,
          Map.get(found, relationship.destination_attribute)
        )
        |> Ash.Changeset.set_context(join_relationship.context)
        |> Ash.create(return_notifications?: true)
        |> case do
          {:ok, _created, notifications} ->
            case key do
              :relate ->
                {:ok, [found | current_value], notifications, [found]}

              :relate_and_update ->
                case handle_update(
                       record,
                       current_value,
                       relationship,
                       found,
                       input,
                       changeset,
                       actor,
                       opts
                     ) do
                  {:ok, new_value, update_notifications, used} ->
                    {:ok, new_value, update_notifications ++ notifications, used}

                  {:error, error} ->
                    {:error, error}
                end
            end

          {:error, error} ->
            {:error, error}
        end

      type when type in [:has_many, :has_one] ->
        {found, input} =
          cond do
            is_struct(input, relationship.destination) ->
              {input, %{}}

            is_struct(input) ->
              {found, Map.from_struct(input)}

            true ->
              {found, input}
          end

        input =
          Map.take(
            input,
            Enum.to_list(
              Ash.Resource.Info.action_inputs(relationship.destination, create_or_update)
            )
          )

        found
        |> Ash.Changeset.new()
        |> Ash.Changeset.set_context(%{
          accessing_from: %{source: relationship.source, name: relationship.name}
        })
        |> Ash.Changeset.for_update(create_or_update, input,
          actor: actor,
          tenant: changeset.tenant,
          authorize?: opts[:authorize?],
          skip_unknown_inputs: Map.keys(input),
          domain: domain(changeset, relationship)
        )
        |> maybe_force_change_attribute(
          relationship,
          :destination_attribute,
          Map.get(record, relationship.source_attribute)
        )
        |> Ash.Changeset.set_context(relationship.context)
        |> Ash.update(return_notifications?: true)
        |> case do
          {:ok, updated, notifications} ->
            {:ok, [updated | current_value], notifications, [updated]}

          {:error, error} ->
            {:error, error}
        end

      :belongs_to ->
        {:ok, [found | current_value], [], [found]}
    end
  end

  defp do_handle_create(record, current_value, relationship, input, changeset, actor, index, opts) do
    case opts[:on_no_match] do
      :error ->
        if opts[:on_lookup] != :ignore do
          {:error,
           NotFound.exception(
             primary_key: input,
             resource: relationship.destination
           )}
        else
          {:error,
           InvalidRelationship.exception(
             relationship: relationship.name,
             message: "changes would create a new related record"
           )}
        end

      {:create, action_name} ->
        case changeset.context[:private][:belongs_to_manage_created][relationship.name][index] do
          nil ->
            created =
              if is_struct(input, relationship.destination) do
                {:ok, input, []}
              else
                input =
                  if is_struct(input) do
                    Map.from_struct(input)
                  else
                    input
                  end

                input =
                  Map.take(
                    input,
                    Enum.to_list(
                      Ash.Resource.Info.action_inputs(relationship.destination, action_name)
                    )
                  )

                relationship.destination
                |> Ash.Changeset.new()
                |> Ash.Changeset.set_context(%{
                  accessing_from: %{source: relationship.source, name: relationship.name}
                })
                |> Ash.Changeset.for_create(action_name, input,
                  require?: false,
                  actor: actor,
                  tenant: changeset.tenant,
                  authorize?: opts[:authorize?],
                  skip_unknown_inputs: Map.keys(input),
                  domain: domain(changeset, relationship)
                )
                |> maybe_force_change_attribute(
                  relationship,
                  :destination_attribute,
                  Map.get(record, relationship.source_attribute)
                )
                |> Ash.Changeset.set_context(relationship.context)
                |> Ash.create(return_notifications?: true)
              end

            case created do
              {:ok, created, notifications} ->
                {:ok, [created | current_value], notifications, [created]}

              {:error, error} ->
                {:error, error}
            end

          created ->
            {:ok, [created | current_value], [], [created]}
        end

      {:create, action_name, join_action_name, params} ->
        join_keys = params ++ Enum.map(params, &to_string/1)

        input =
          cond do
            is_struct(input, relationship.destination) ->
              input

            is_struct(input) ->
              Map.from_struct(input)

            true ->
              input
          end

        {join_params, regular_params} = split_join_keys(input, join_keys)

        created =
          if is_struct(input, relationship.destination) do
            {:ok, input, []}
          else
            regular_params =
              Map.take(
                regular_params,
                Enum.to_list(
                  Ash.Resource.Info.action_inputs(relationship.destination, action_name)
                )
              )

            relationship.destination
            |> Ash.Changeset.new()
            |> Ash.Changeset.set_context(%{
              accessing_from: %{source: relationship.source, name: relationship.name}
            })
            |> Ash.Changeset.for_create(action_name, regular_params,
              require?: false,
              authorize?: opts[:authorize?],
              actor: actor,
              tenant: changeset.tenant,
              skip_unknown_inputs: Map.keys(regular_params),
              domain: domain(changeset, relationship)
            )
            |> Ash.Changeset.set_context(relationship.context)
            |> Ash.create(return_notifications?: true)
          end

        case created do
          {:ok, created, regular_notifications} ->
            join_relationship =
              Ash.Resource.Info.relationship(relationship.source, relationship.join_relationship)

            relationship.through
            |> Ash.Changeset.new()
            |> Ash.Changeset.set_context(%{
              accessing_from: %{source: join_relationship.source, name: join_relationship.name}
            })
            |> Ash.Changeset.for_create(join_action_name, join_params,
              require?: false,
              authorize?: opts[:authorize?],
              actor: actor,
              tenant: changeset.tenant,
              skip_unknown_inputs: Map.keys(join_params),
              domain: domain(changeset, join_relationship)
            )
            |> maybe_force_change_attribute(
              relationship,
              :source_attribute_on_join_resource,
              Map.get(record, relationship.source_attribute)
            )
            |> maybe_force_change_attribute(
              relationship,
              :destination_attribute_on_join_resource,
              Map.get(created, relationship.destination_attribute)
            )
            |> Ash.Changeset.set_context(join_relationship.context)
            |> Ash.create(return_notifications?: true)
            |> case do
              {:ok, _join_row, notifications} ->
                {:ok, [created | current_value], regular_notifications ++ notifications,
                 [created]}

              {:error, error} ->
                {:error, error}
            end

          {:error, error} ->
            {:error, error}
        end

      ignore when ignore in [:ignore, :match] ->
        {:ok, current_value, [], [input]}
    end
  end

  # credo:disable-for-next-line Credo.Check.Refactor.Nesting
  defp handle_update(
         source_record,
         current_value,
         relationship,
         match,
         input,
         changeset,
         actor,
         opts
       ) do
    domain = domain(changeset, relationship)

    case opts[:on_match] do
      # :create case is handled when determining updates/creates

      :error ->
        {:error,
         InvalidRelationship.exception(
           relationship: relationship.name,
           message: "changes would update a record"
         )}

      :ignore ->
        {:ok, [match | current_value], [], [match]}

      :missing ->
        {:ok, current_value, [], []}

      {:destroy, action_name} ->
        case destroy_data(
               source_record,
               match,
               domain,
               actor,
               opts,
               action_name,
               changeset,
               relationship
             ) do
          {:ok, notifications} ->
            {:ok, current_value, notifications, [match]}

          {:error, error} ->
            {:error, error}
        end

      {:unrelate, action_name} ->
        case unrelate_data(
               source_record,
               match,
               domain,
               actor,
               opts,
               action_name,
               changeset,
               relationship
             ) do
          {:ok, notifications} ->
            {:ok, current_value, notifications, [match]}

          {:error, error} ->
            {:error, error}
        end

      {:update, action_name} ->
        {match, input} =
          cond do
            is_struct(input, relationship.destination) ->
              {input, %{}}

            is_struct(input) ->
              Map.from_struct(input)

            true ->
              {match, input}
          end

        input =
          Map.take(
            input,
            Enum.to_list(Ash.Resource.Info.action_inputs(relationship.destination, action_name))
          )

        match
        |> Ash.Changeset.new()
        |> Ash.Changeset.set_context(%{
          accessing_from: %{source: relationship.source, name: relationship.name}
        })
        |> Ash.Changeset.for_update(action_name, input,
          actor: actor,
          tenant: changeset.tenant,
          authorize?: opts[:authorize?],
          domain: domain,
          skip_unknown_inputs: Map.keys(input)
        )
        |> Ash.Changeset.set_context(relationship.context)
        |> Ash.update(return_notifications?: true)
        |> case do
          {:ok, updated, update_notifications} ->
            {:ok, [updated | current_value], update_notifications, [match]}

          {:error, error} ->
            {:error, error}
        end

      {:update, action_name, join_action_name, params} ->
        join_keys = params ++ Enum.map(params, &to_string/1)
        {join_params, regular_params} = split_join_keys(input, join_keys)

        {match, regular_params} =
          cond do
            is_struct(regular_params, relationship.destination) ->
              {regular_params, %{}}

            is_struct(regular_params) ->
              {match, Map.from_struct(regular_params)}

            true ->
              {match, regular_params}
          end

        regular_params =
          Map.take(
            regular_params,
            Enum.to_list(Ash.Resource.Info.action_inputs(relationship.destination, action_name))
          )

        source_value = Map.get(source_record, relationship.source_attribute)

        update_result =
          if action_name do
            match
            |> Ash.Changeset.new()
            |> Ash.Changeset.set_context(%{
              accessing_from: %{source: relationship.source, name: relationship.name}
            })
            |> Ash.Changeset.for_update(action_name, regular_params,
              actor: actor,
              tenant: changeset.tenant,
              authorize?: opts[:authorize?],
              domain: domain,
              skip_unknown_inputs: Map.keys(regular_params)
            )
            |> Ash.Changeset.set_context(relationship.context)
            |> Ash.update(return_notifications?: true)
          else
            {:ok, match, []}
          end

        case update_result do
          {:ok, updated, update_notifications} ->
            destination_value = Map.get(updated, relationship.destination_attribute)

            join_relationship =
              Ash.Resource.Info.relationship(relationship.source, relationship.join_relationship)

            relationship.through
            |> Ash.Query.set_context(%{
              accessing_from: %{source: join_relationship.source, name: join_relationship.name}
            })
            |> Ash.Query.filter(
              ^ref(relationship.source_attribute_on_join_resource) == ^source_value
            )
            |> Ash.Query.filter(
              ^ref(relationship.destination_attribute_on_join_resource) == ^destination_value
            )
            |> Ash.Query.set_context(join_relationship.context)
            |> Ash.Query.limit(1)
            |> Ash.Query.set_tenant(changeset.tenant)
            |> Ash.read_one(
              domain: domain(changeset, join_relationship),
              authorize?: opts[:authorize?],
              actor: actor
            )
            |> case do
              {:ok, result} ->
                join_relationship =
                  Ash.Resource.Info.relationship(
                    relationship.source,
                    relationship.join_relationship
                  )

                result
                |> Ash.Changeset.new()
                |> Ash.Changeset.set_context(%{
                  accessing_from: %{
                    source: join_relationship.source,
                    name: join_relationship.name
                  }
                })
                |> Ash.Changeset.for_update(join_action_name, join_params,
                  actor: actor,
                  tenant: changeset.tenant,
                  authorize?: opts[:authorize?],
                  skip_unknown_inputs: Map.keys(join_params),
                  domain: domain(changeset, join_relationship)
                )
                |> Ash.Changeset.set_context(join_relationship.context)
                |> Ash.update(return_notifications?: true)
                # credo:disable-for-next-line Credo.Check.Refactor.Nesting
                |> case do
                  {:ok, _updated_join, join_update_notifications} ->
                    {:ok, [updated | current_value],
                     update_notifications ++ join_update_notifications, [updated]}

                  {:error, error} ->
                    {:error, error}
                end

              {:error, error} ->
                {:error, error}
            end

          {:error, error} ->
            {:error, error}
        end
    end
  end

  defp find_match(current_value, input, pkeys, relationship, force_has_one? \\ false)

  defp find_match(%Ash.NotLoaded{}, _input, _pkeys, _relationship, _force_has_one?) do
    nil
  end

  defp find_match([match], _input, _pkeys, %{cardinality: :one}, true) do
    match
  end

  defp find_match(current_value, input, pkeys, relationship, _force_has_one?) do
    Enum.find(current_value, fn current_value ->
      Enum.any?(pkeys, fn pkey ->
        matches?(current_value, input, pkey, relationship)
      end)
    end)
  end

  defp matches?(current_value, input, pkey, relationship) do
    if relationship && relationship.type in [:has_one, :has_many] &&
         relationship.destination_attribute in pkey do
      Enum.all?(pkey, fn field ->
        attr = Ash.Resource.Info.attribute(relationship.destination, field)

        if field == relationship.destination_attribute do
          if is_struct(input) do
            do_matches?(current_value, input, field, attr.type)
          else
            # We know that it will be the same as all other records in this relationship
            # (because thats how has_one and has_many relationships work), so we
            # can assume its the same as the current value
            true
          end
        else
          do_matches?(current_value, input, field, attr.type)
        end
      end)
    else
      Enum.all?(pkey, fn field ->
        attr = Ash.Resource.Info.attribute(relationship.destination, field)
        do_matches?(current_value, input, field, attr.type)
      end)
    end
  end

  defp do_matches?(current_value, input, field, type) do
    with {:ok, current_val} when not is_nil(current_val) <- Map.fetch(current_value, field),
         {:ok, input_val} when not is_nil(input_val) <- fetch_field(input, field),
         {:ok, current_val} <- Ash.Type.cast_input(type, current_val),
         {:ok, input_val} <- Ash.Type.cast_input(type, input_val) do
      Ash.Type.equal?(type, current_val, input_val)
    else
      _ ->
        false
    end
  end

  defp split_join_keys(%_{__metadata__: metadata} = input, _join_keys) do
    {metadata[:join_keys] || %{}, input}
  end

  defp split_join_keys(input, []) do
    {%{}, input}
  end

  defp split_join_keys(input, :*) do
    {input, %{}}
  end

  defp split_join_keys(input, join_keys) do
    Map.split(input, join_keys ++ Enum.map(join_keys, &to_string/1))
  end

  defp fetch_field(input, field) do
    case Map.fetch(input, field) do
      {:ok, value} ->
        {:ok, value}

      :error ->
        Map.fetch(input, to_string(field))
    end
  end

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
    domain = domain(changeset, relationship)
    pkey = Ash.Resource.Info.primary_key(relationship.destination)

    original_value
    |> List.wrap()
    |> Enum.reject(&find_match(all_used, &1, [pkey], relationship))
    |> Enum.reduce_while(
      {:ok, current_value, []},
      fn record, {:ok, current_value, all_notifications} ->
        case opts[:on_missing] do
          :ignore ->
            {:cont, {:ok, [record | current_value], []}}

          {:destroy, action_name, join_action_name} ->
            source_value = Map.get(source_record, relationship.source_attribute)
            destination_value = Map.get(record, relationship.destination_attribute)

            join_relationship =
              Ash.Resource.Info.relationship(relationship.source, relationship.join_relationship)

            relationship.through
            |> Ash.Query.set_context(%{
              accessing_from: %{source: join_relationship.source, name: join_relationship.name}
            })
            |> Ash.Query.filter(
              ^ref(relationship.source_attribute_on_join_resource) == ^source_value
            )
            |> Ash.Query.filter(
              ^ref(relationship.destination_attribute_on_join_resource) == ^destination_value
            )
            |> Ash.Query.limit(1)
            |> Ash.Query.set_tenant(changeset.tenant)
            |> Ash.Query.set_context(join_relationship.context)
            |> Ash.Query.do_filter(relationship.filter, parent_stack: relationship.source)
            |> Ash.Query.sort(relationship.sort, prepend?: true)
            |> Ash.read_one(
              domain: domain(changeset, join_relationship),
              authorize?: opts[:authorize?],
              actor: actor
            )
            |> case do
              {:ok, result} ->
                result
                |> Ash.Changeset.new()
                |> Ash.Changeset.set_context(%{
                  accessing_from: %{
                    source: join_relationship.source,
                    name: join_relationship.name
                  }
                })
                |> Ash.Changeset.for_destroy(
                  join_action_name,
                  %{},
                  actor: actor,
                  tenant: changeset.tenant,
                  authorize?: opts[:authorize?],
                  domain: domain(changeset, join_relationship)
                )
                |> Ash.Changeset.set_context(join_relationship.context)
                |> Ash.destroy(return_notifications?: true)
                |> case do
                  {:ok, join_notifications} ->
                    notifications = join_notifications ++ all_notifications

                    record
                    |> Ash.Changeset.new()
                    |> Ash.Changeset.set_context(%{
                      accessing_from: %{source: relationship.source, name: relationship.name}
                    })
                    |> Ash.Changeset.for_destroy(action_name, %{},
                      actor: actor,
                      tenant: changeset.tenant,
                      authorize?: opts[:authorize?],
                      domain: domain
                    )
                    |> Ash.Changeset.set_context(relationship.context)
                    |> Ash.destroy(return_notifications?: true)
                    # credo:disable-for-next-line Credo.Check.Refactor.Nesting
                    |> case do
                      {:ok, destroy_destination_notifications} ->
                        {:cont,
                         {:ok, current_value,
                          notifications ++
                            all_notifications ++ destroy_destination_notifications}}

                      {:error, error} ->
                        {:halt, {:error, error}}
                    end

                  {:error, error} ->
                    {:halt, {:error, error}}
                end

              {:error, error} ->
                {:halt, {:error, error}}
            end

          {:destroy, action_name} ->
            record
            |> Ash.Changeset.new()
            |> Ash.Changeset.set_context(%{
              accessing_from: %{source: relationship.source, name: relationship.name}
            })
            |> Ash.Changeset.for_destroy(action_name, %{},
              actor: actor,
              tenant: changeset.tenant,
              authorize?: opts[:authorize?],
              domain: domain
            )
            |> Ash.Changeset.set_context(relationship.context)
            |> Ash.destroy(return_notifications?: true)
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
                message: "changes would destroy a record"
              )}}

          {:unrelate, action_name} ->
            case unrelate_data(
                   source_record,
                   record,
                   domain,
                   actor,
                   opts,
                   action_name,
                   changeset,
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
         _domain,
         actor,
         opts,
         action_name,
         changeset,
         %{type: :many_to_many} = relationship
       ) do
    tenant = changeset.tenant

    source_value = Map.get(source_record, relationship.source_attribute)
    destination_value = Map.get(record, relationship.destination_attribute)

    join_relationship =
      Ash.Resource.Info.relationship(relationship.source, relationship.join_relationship)

    relationship.through
    |> Ash.Query.set_context(%{
      accessing_from: %{source: join_relationship.source, name: join_relationship.name}
    })
    |> Ash.Query.filter(^ref(relationship.source_attribute_on_join_resource) == ^source_value)
    |> Ash.Query.filter(
      ^ref(relationship.destination_attribute_on_join_resource) == ^destination_value
    )
    |> Ash.Query.limit(1)
    |> Ash.Query.set_tenant(tenant)
    |> Ash.Query.set_context(join_relationship.context)
    |> Ash.read_one(
      authorize?: opts[:authorize?],
      actor: actor,
      domain: domain(changeset, join_relationship)
    )
    |> case do
      {:ok, result} ->
        result
        |> Ash.Changeset.new()
        |> Ash.Changeset.set_context(%{
          accessing_from: %{source: join_relationship.source, name: join_relationship.name}
        })
        |> Ash.Changeset.for_destroy(action_name, %{},
          actor: actor,
          authorize?: opts[:authorize?],
          domain: domain(changeset, join_relationship)
        )
        |> Ash.Changeset.set_context(join_relationship.context)
        |> Ash.Changeset.set_tenant(tenant)
        |> Ash.destroy(return_notifications?: true)
        |> case do
          {:ok, notifications} ->
            {:ok, notifications}

          {:error, error} ->
            {:error, error}
        end

      {:error, error} ->
        {:error, error}
    end
  end

  defp unrelate_data(
         _source_record,
         record,
         domain,
         actor,
         opts,
         action_name,
         changeset,
         %{type: type} = relationship
       )
       when type in [:has_many, :has_one] do
    tenant = changeset.tenant

    record
    |> Ash.Changeset.new()
    |> Ash.Changeset.set_context(%{
      accessing_from: %{source: relationship.source, name: relationship.name, unrelating?: true}
    })
    |> Ash.Changeset.for_update(action_name, %{},
      authorize?: opts[:authorize?],
      actor: actor,
      domain: domain
    )
    |> maybe_force_change_attribute(relationship, :destination_attribute, nil)
    |> Ash.Changeset.set_context(relationship.context)
    |> Ash.Changeset.set_tenant(tenant)
    |> Ash.update(return_notifications?: true)
    |> case do
      {:ok, _unrelated, notifications} ->
        {:ok, notifications}

      {:error, error} ->
        {:error, error}
    end
  end

  defp unrelate_data(
         _source_record,
         _record,
         _domain,
         _actor,
         _opts,
         _action_name,
         _changeset,
         %{type: :belongs_to}
       ) do
    {:ok, []}
  end

  defp destroy_data(
         source_record,
         record,
         _domain,
         actor,
         opts,
         action_name,
         changeset,
         %{type: :many_to_many} = relationship
       ) do
    tenant = changeset.tenant

    source_value = Map.get(source_record, relationship.source_attribute)
    destination_value = Map.get(record, relationship.destination_attribute)

    join_relationship =
      Ash.Resource.Info.relationship(relationship.source, relationship.join_relationship)

    relationship.through
    |> Ash.Query.set_context(%{
      accessing_from: %{source: join_relationship.source, name: join_relationship.name}
    })
    |> Ash.Query.filter(^ref(relationship.source_attribute_on_join_resource) == ^source_value)
    |> Ash.Query.filter(
      ^ref(relationship.destination_attribute_on_join_resource) == ^destination_value
    )
    |> Ash.Query.limit(1)
    |> Ash.Query.set_tenant(tenant)
    |> Ash.read_one(
      authorize?: opts[:authorize?],
      actor: actor,
      domain: domain(changeset, join_relationship)
    )
    |> case do
      {:ok, result} ->
        result
        |> Ash.Changeset.new()
        |> Ash.Changeset.set_context(%{
          accessing_from: %{source: join_relationship.source, name: join_relationship.name}
        })
        |> Ash.Changeset.for_destroy(action_name, %{},
          actor: actor,
          authorize?: opts[:authorize?],
          domain: domain(changeset, join_relationship)
        )
        |> Ash.Changeset.set_context(join_relationship.context)
        |> Ash.Changeset.set_tenant(tenant)
        |> Ash.destroy(return_notifications?: true)
        |> case do
          {:ok, notifications} ->
            {:ok, notifications}

          {:error, error} ->
            {:error, error}
        end

      {:error, error} ->
        {:error, error}
    end
  end

  defp destroy_data(
         _source_record,
         record,
         domain,
         actor,
         opts,
         action_name,
         changeset,
         relationship
       ) do
    tenant = changeset.tenant

    record
    |> Ash.Changeset.new()
    |> Ash.Changeset.set_context(%{
      accessing_from: %{source: relationship.source, name: relationship.name}
    })
    |> Ash.Changeset.for_destroy(action_name, %{},
      actor: actor,
      authorize?: opts[:authorize?],
      domain: domain
    )
    |> Ash.Changeset.set_context(relationship.context)
    |> Ash.Changeset.set_tenant(tenant)
    |> Ash.destroy(return_notifications?: true)
  end
end
