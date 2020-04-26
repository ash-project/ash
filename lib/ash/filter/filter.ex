defmodule Ash.Filter do
  defstruct [
    :api,
    :resource,
    :not,
    ors: [],
    attributes: %{},
    relationships: %{},
    requests: [],
    path: [],
    errors: [],
    impossible?: false
  ]

  alias Ash.Engine.Request
  alias Ash.Filter.Merge

  @type t :: %__MODULE__{
          api: Ash.api(),
          resource: Ash.resource(),
          ors: list(%__MODULE__{} | nil),
          not: %__MODULE__{} | nil,
          attributes: Keyword.t(),
          relationships: Map.t(),
          path: list(atom),
          impossible?: boolean,
          errors: list(String.t()),
          requests: list(Ash.Engine.Request.t())
        }

  @predicates %{
    not_eq: Ash.Filter.NotEq,
    not_in: Ash.Filter.NotIn,
    eq: Ash.Filter.Eq,
    in: Ash.Filter.In,
    and: Ash.Filter.And,
    or: Ash.Filter.Or
  }

  @spec parse(
          Ash.resource(),
          Keyword.t(),
          Ash.api(),
          relationship_path :: list(atom)
        ) :: t()
  # The `api` argument is here primarily because the requests
  # need to have the `api`.
  def parse(resource, filter, api \\ nil, path \\ []) do
    parsed_filter =
      filter
      |> do_parse(%Ash.Filter{resource: resource, api: api, path: path})
      |> lift_ors()
      |> add_not_filter_info()

    source =
      case path do
        [] -> "filter"
        path -> "related #{Enum.join(path, ".")} filter"
      end

    if path == [] do
      parsed_filter
    else
      request =
        Ash.Engine.Request.new(
          resource: resource,
          api: api,
          rules: Ash.primary_action(resource, :read).rules,
          filter: parsed_filter,
          path: [:filter, path],
          resolve_when_fetch_only?: false,
          data:
            Ash.Engine.Request.resolve(
              [[:filter, path, :filter]],
              fn %{filter: %{^path => %{filter: filter}}} ->
                query = Ash.DataLayer.resource_to_query(resource)

                case Ash.DataLayer.filter(query, filter, resource) do
                  {:ok, filtered_query} ->
                    Ash.DataLayer.run_query(filtered_query, resource)

                  {:error, error} ->
                    {:error, error}
                end
              end
            ),
          action_type: :read,
          strict_access?: !primary_key_filter?(parsed_filter),
          relationship: path,
          name: source
        )

      add_request(
        parsed_filter,
        request
      )
    end
  end

  def primary_key_filter?(nil), do: false

  def primary_key_filter?(%{relationships: relationships}) when relationships not in [%{}, nil] do
    false
  end

  def primary_key_filter?(%{attributes: attributes, not: not_filter, ors: ors, resource: resource}) do
    not_filter_is_primary_key_filter? =
      if not_filter do
        primary_key_filter?(not_filter)
      else
        true
      end

    attributes_primary_key_filter?(attributes, resource) && Enum.all?(ors, &primary_key_filter?/1) &&
      not_filter_is_primary_key_filter?
  end

  defp attributes_primary_key_filter?(attributes, resource) do
    resource
    |> Ash.primary_key()
    |> Enum.all?(fn pkey_field ->
      case Map.fetch(attributes, pkey_field) do
        {:ok, value} ->
          exact_match_filter?(value)

        :error ->
          false
      end
    end)
  end

  defp exact_match_filter?(%Ash.Filter.Eq{}), do: true

  defp exact_match_filter?(%Ash.Filter.And{left: left, right: right}) do
    exact_match_filter?(left) && exact_match_filter?(right)
  end

  defp exact_match_filter?(%Ash.Filter.Or{left: left, right: right}) do
    exact_match_filter?(left) && exact_match_filter?(right)
  end

  defp exact_match_filter?(%Ash.Filter.In{}), do: true

  defp exact_match_filter?(_), do: false

  def optional_paths(filter) do
    filter
    |> do_optional_paths()
    |> Enum.uniq()
  end

  defp do_optional_paths(%{relationships: relationships, requests: requests, ors: ors})
       when relationships == %{} and ors in [[], nil] do
    Enum.map(requests, fn request ->
      request.path
    end)
  end

  defp do_optional_paths(%{ors: [first | rest]} = filter) do
    do_optional_paths(first) ++ do_optional_paths(%{filter | ors: rest})
  end

  defp do_optional_paths(%{relationships: relationships} = filter) when is_map(relationships) do
    relationship_paths =
      Enum.flat_map(relationships, fn {_, value} ->
        do_optional_paths(value)
      end)

    relationship_paths ++ do_optional_paths(%{filter | relationships: %{}})
  end

  def request_filter_for_fetch(filter, data) do
    filter
    |> optional_paths()
    |> paths_and_data(data)
    |> most_specific_paths()
    |> Enum.reduce(filter, fn {path, %{data: related_data}}, filter ->
      [:filter, relationship_path] = path

      filter
      |> add_records_to_relationship_filter(
        relationship_path,
        List.wrap(related_data)
      )
      |> lift_impossibility()
    end)
  end

  defp most_specific_paths(paths_and_data) do
    Enum.reject(paths_and_data, fn {path, _} ->
      Enum.any?(paths_and_data, &path_is_more_specific?(path, &1))
    end)
  end

  # I don't think this is a possibility
  defp path_is_more_specific?([], []), do: false
  defp path_is_more_specific?(_, []), do: true
  # first element of the search matches first element of candidate
  defp path_is_more_specific?([part | rest], [part | candidate_rest]) do
    path_is_more_specific?(rest, candidate_rest)
  end

  defp path_is_more_specific?(_, _), do: false

  defp paths_and_data(paths, data) do
    Enum.flat_map(paths, fn path ->
      case Request.fetch_nested_value(data, path) do
        {:ok, related_data} -> [{path, related_data}]
        :error -> []
      end
    end)
  end

  # Cosimplification reduces the rates of false negatives
  # and false positives by ensuring filters are stated in the same
  # way
  def cosimplify(left, right, state \\ %{}, first? \\ true) do
    {new_left, state} = do_cosimplify(left, state, first?)

    {new_right, state} = do_cosimplify(right, state, first?)

    if new_left == left && new_right == right do
      {new_left, new_right}
    else
      cosimplify(new_left, new_right, state, false)
    end
  end

  defp do_cosimplify(filter, state, first?) do
    state =
      if first? do
        Enum.reduce(filter.attributes, state, fn {field, value}, state ->
          state
          |> Map.put_new(filter.path, %{})
          |> Map.update!(filter.path, fn paths ->
            paths
            |> Map.put_new(field, [])
            |> Map.update!(field, fn values ->
              [value | values]
            end)
          end)
        end)
      else
        state
      end

    {new_attrs, state} =
      Enum.reduce(filter.attributes, {%{}, state}, fn {field, value}, {attributes, state} ->
        {new_value, state} = cosimplify_value(value, state, filter.path, field)

        {Map.put(attributes, field, new_value), state}
      end)

    {new_relationships, state} =
      Enum.reduce(filter.relationships, {%{}, state}, fn {relationship, related_filter},
                                                         {relationships, state} ->
        {new_relationship_filter, state} = do_cosimplify(related_filter, state, first?)

        {Map.put(relationships, relationship, new_relationship_filter), state}
      end)

    {new_not, state} =
      if filter.not do
        do_cosimplify(filter.not, state, first?)
      else
        {filter.not, state}
      end

    {new_ors, state} =
      Enum.reduce(filter.ors, {[], state}, fn or_filter, {ors, state} ->
        {new_or, state} = do_cosimplify(or_filter, state, first?)

        {[new_or | ors], state}
      end)

    {%{
       filter
       | attributes: new_attrs,
         relationships: new_relationships,
         not: new_not,
         ors: Enum.reverse(new_ors)
     }, state}
  end

  defp cosimplify_value(value, state, path, field) do
    other_values = get_in(state, [path, field])

    {new_other_values, new_value, _found?} =
      Enum.reduce(other_values, {other_values, value, false}, fn
        other_value, {other_values, value, true} ->
          {[other_value | other_values], value, true}

        other_value, {other_values, value, false} ->
          case do_cosimplify_value(value, other_value) do
            {:ok, cosimplified, cosimplified_other} ->
              {[cosimplified_other | other_values], cosimplified, true}

            :error ->
              {[other_value | other_values], value, false}
          end
      end)

    {new_value, put_in(state, [path, field], Enum.reverse(new_other_values))}
  end

  defp do_cosimplify_value(value, value), do: :error

  defp do_cosimplify_value(%struct{left: left, right: right} = compound, other_value)
       when struct in [Ash.Filter.And, Ash.Filter.Or] do
    case do_cosimplify_value(left, other_value) do
      {:ok, cosimplified, cosimplified_other} ->
        {:ok, %{compound | left: cosimplified, right: right}, cosimplified_other}

      :error ->
        case do_cosimplify_value(right, other_value) do
          {:ok, cosimplified, cosimplified_other} ->
            {:ok, %{compound | left: left, right: cosimplified}, cosimplified_other}

          :error ->
            :error
        end
    end
  end

  defp do_cosimplify_value(value, %struct{left: left, right: right} = other_compound)
       when struct in [Ash.Filter.And, Ash.Filter.Or] do
    case do_cosimplify_value(value, left) do
      {:ok, cosimplified, cosimplified_other} ->
        {:ok, cosimplified, %{other_compound | left: cosimplified_other, right: right}}

      :error ->
        case do_cosimplify_value(value, right) do
          {:ok, cosimplified, cosimplified_other} ->
            {:ok, cosimplified, %{other_compound | left: left, right: cosimplified_other}}

          :error ->
            :error
        end
    end
  end

  defp do_cosimplify_value(
         %eq_struct{value: value} = eq,
         %in_struct{values: values} = in_clause
       )
       when eq_struct in [Ash.Filter.Eq, Ash.Filter.NotEq] and
              in_struct in [Ash.Filer.In, Ash.Filter.NotIn] do
    if value in values do
      new_in = %{in_clause | values: Enum.reject(values, &(&1 == value))}
      {:ok, eq, Ash.Filter.Or.prebuilt_new(eq, new_in)}
    else
      :error
    end
  end

  defp do_cosimplify_value(
         %left_struct{values: left_values} = left,
         %right_struct{
           values: right_values
         } = right
       )
       when left_struct in [Ash.Filter.In, Ash.Filter.NotIn] and
              right_struct in [Ash.Filter.In, Ash.Filter.NotIn] do
    left_mapset = MapSet.new(left_values)
    right_mapset = MapSet.new(right_values)

    commonalities =
      left_mapset
      |> MapSet.intersection(right_mapset)

    cond do
      MapSet.equal?(left_mapset, right_mapset) ->
        {:ok, %{left | values: Enum.sort(left_values)}, %{right | right: Enum.sort(right_values)}}

      MapSet.size(commonalities) > 0 ->
        common_in = %{left | values: MapSet.to_list(commonalities)}

        left_exclusive_in = %{
          left
          | values: MapSet.to_list(MapSet.difference(left_mapset, right_mapset))
        }

        new_left =
          if Enum.empty?(left_exclusive_in.values) do
            common_in
          else
            Ash.Filter.Or.prebuilt_new(left_exclusive_in, common_in)
          end

        right_exclusive_in = %{
          right
          | values: MapSet.to_list(MapSet.difference(right_mapset, left_mapset))
        }

        new_right =
          if Enum.empty?(right_exclusive_in.values) do
            common_in
          else
            Ash.Filter.Or.prebuilt_new(right_exclusive_in, common_in)
          end

        {:ok, new_left, new_right}

      true ->
        :error
    end
  end

  defp do_cosimplify_value(_, _), do: :error

  # @predicates %{
  #   not_eq: Ash.Filter.NotEq,
  #   not_in: Ash.Filter.NotIn,
  #   eq: Ash.Filter.Eq,
  #   in: Ash.Filter.In,
  # and
  # or
  # }

  # defp do_cosimplify(left, right) do
  #   Enum.reduce(left.attributes, fn {attr_name, clause} ->
  #     clause.__module__.cosimplify(right)
  #   end)
  # end

  @doc """
  Returns true if the second argument is a strict subset (always returns the same or less data) of the first
  """
  def strict_subset_of(nil, nil), do: true

  def strict_subset_of(_, nil), do: false

  def strict_subset_of(%{resource: resource}, %{resource: other_resource})
      when resource != other_resource,
      do: false

  def strict_subset_of(filter, candidate) do
    # IO.inspect(filter, label: "filter")
    # IO.inspect(candidate, label: "candidate")
    {filter, candidate} = cosimplify(filter, candidate)
    # IO.inspect(filter, label: "cosimplified")
    # IO.inspect(candidate, label: "cosimplified")
    Ash.Authorization.SatSolver.strict_filter_subset(filter, candidate)
  end

  def strict_subset_of?(filter, candidate) do
    strict_subset_of(filter, candidate) == true
  end

  # This is insufficient and will yield false negatives
  # defp ors_contained?(filter, candidate) do
  # end

  # defp not_doesnt_contain?(filter, candidate) do

  # end

  # left:
  # a in [1, 2, 3, 5] or a = 4
  # right:
  # a in [1, 2] and not(a = 4 or a = 5)
  #
  #

  # (a = 1 or a = 2) and not(a = 1 and b = 2)
  # (a = 2) and (b = 2)
  # remaining_not_negated SHOULD BE: (a = 1)

  # defp do_strict_subset_of?(filter, candidate) do
  #   # TODO: Finish this!
  #   unless filter.ors in [[], nil], do: raise("Can't do ors contains yet")
  #   unless candidate.ors in [[], nil], do: raise("Can't do ors contains yet")
  #   unless candidate.not in [[], nil], do: raise("Can't do not contains yet")

  #   {negated?, new_not_filter} =
  #     if filter.not do
  #       do_strict_subset_of?(filter.not, candidate)
  #     else
  #       {false, nil}
  #     end

  #   filter = %{filter | not: new_not_filter}

  #   if negated? do
  #     {false, candidate}
  #   else
  #     remaining_attributes = uncontained_attributes(filter, candidate)
  #     remaining_relationships = uncontained_relationships(filter, candidate)

  #     if remaining_attributes == %{} and remaining_relationships == %{} do
  #       {true, %{candidate | attributes: %{}, relationships: %{}}}
  #     else
  #       {false,
  #        %{candidate | attributes: remaining_attributes, relationships: remaining_relationships}}
  #     end
  #   end
  # end

  # defp uncontained_attributes(filter, candidate) do
  #   candidate.attributes
  #   |> Enum.reject(fn {attr, predicate} ->
  #     contains_attribute?(filter, attr, predicate)
  #   end)
  #   |> Enum.into(%{})
  # end

  # defp uncontained_relationships(filter, candidate) do
  #   candidate.relationships
  #   |> Enum.reject(fn {relationship, relationship_filter} ->
  #     contains_relationship?(filter, relationship, relationship_filter)
  #   end)
  #   |> Enum.into(%{})
  # end

  defp add_records_to_relationship_filter(filter, [], records) do
    case Ash.Actions.PrimaryKeyHelpers.values_to_primary_key_filters(filter.resource, records) do
      {:error, _error} ->
        # TODO: We should get this error out somehow?
        filter

      {:ok, []} ->
        if filter.ors in [[], nil] do
          # TODO: We should probably include some kind of filter that *makes* it immediately impossible
          # that way, if the data layer doesn't check impossibility they will run the simpler query,
          # like for each pkey field say `[field: [in: []]]`
          %{filter | impossible?: true}
        else
          filter
        end

      {:ok, [single]} ->
        do_parse(single, filter)

      {:ok, many} ->
        do_parse([or: many], filter)
    end
  end

  defp add_records_to_relationship_filter(filter, [relationship | rest] = path, records) do
    filter
    |> Map.update!(:relationships, fn relationships ->
      case Map.fetch(relationships, relationship) do
        {:ok, related_filter} ->
          Map.put(
            relationships,
            relationship,
            add_records_to_relationship_filter(related_filter, rest, records)
          )

        :error ->
          relationships
      end
    end)
    |> Map.update!(:ors, fn ors ->
      Enum.map(ors, &add_records_to_relationship_filter(&1, path, records))
    end)
  end

  defp lift_impossibility(filter) do
    with_related_impossibility =
      if Enum.any?(filter.relationships || %{}, fn {_, val} -> Map.get(val, :impossible?) end) do
        Map.put(filter, :impossible?, true)
      else
        filter
      end

    Map.update!(with_related_impossibility, :ors, fn ors ->
      Enum.reject(ors, &Map.get(&1, :impossible?))
    end)
  end

  # defp contains_relationship?(filter, relationship, candidate_relationship_filter) do
  #   filter_with_only_relationship_filters =
  #     case Ash.relationship(filter.resource, relationship) do
  #       %{type: :many_to_many} ->
  #         filter

  #       %{
  #         source_field: source_field,
  #         destination_field: destination_field,
  #         destination: destination
  #       } ->
  #         case filter.attributes do
  #           %{^source_field => attribute_filter_struct} ->
  #             filter
  #             |> Map.update!(:attributes, &Map.delete(&1, source_field))
  #             |> Map.update!(:relationships, fn relationships ->
  #               Map.update(
  #                 relationships,
  #                 relationship,
  #                 %__MODULE__{
  #                   resource: destination,
  #                   api: filter.api,
  #                   attributes: %{
  #                     destination_field => attribute_filter_struct
  #                   }
  #                 },
  #                 fn existing_attribute_filter ->
  #                   Ash.Filter.And.new(
  #                     destination,
  #                     existing_attribute_filter,
  #                     attribute_filter_struct
  #                   )
  #                 end
  #               )
  #             end)

  #           _ ->
  #             filter
  #         end
  #     end

  #   this_filter_contains? =
  #     case filter_with_only_relationship_filters.relationships do
  #       %{^relationship => relationship_filter} ->
  #         strict_subset_of?(relationship_filter, candidate_relationship_filter)

  #       _ ->
  #         false
  #     end

  #   this_filter_contains? ||
  #     Enum.any?(
  #       filter.ors,
  #       &contains_relationship?(&1, relationship, candidate_relationship_filter)
  #     )
  # end

  # defp contains_attribute?(filter, attr, candidate_predicate) do
  #   this_filter_contains? =
  #     case filter.attributes do
  #       %{^attr => predicate} ->
  #         attribute = Ash.attribute(filter.resource, attr)

  #         predicate_strict_subset_of?(attribute, predicate, candidate_predicate)

  #       _ ->
  #         false
  #     end

  #   this_filter_contains? ||
  #     Enum.any?(filter.ors, &contains_attribute?(&1, attr, candidate_predicate))
  # end

  defp add_not_filter_info(filter) do
    case filter.not do
      nil ->
        filter

      not_filter ->
        filter
        |> add_request(not_filter.requests)
        |> add_error(not_filter.errors)
    end
  end

  def predicate_strict_subset_of?(attribute, %left_struct{} = left, right) do
    left_struct.strict_subset_of?(attribute, left, right)
  end

  def add_to_filter(filter, additions) do
    do_parse(additions, filter)
  end

  defp lift_ors(%Ash.Filter{
         ors: [or_filter | rest],
         relationships: rels,
         attributes: attrs,
         errors: errors,
         requests: requests
       })
       when attrs == %{} and rels == %{} do
    or_filter
    |> Map.put(:ors, rest)
    |> add_error(errors)
    |> add_request(requests)
    |> lift_ors()
  end

  defp lift_ors(filter), do: filter

  defp do_parse(filter_statement, %{resource: resource} = filter) do
    Enum.reduce(filter_statement, filter, fn
      {key, value}, filter ->
        cond do
          key == :__impossible__ && value == true ->
            %{filter | impossible?: true}

          key in [:or, :and, :not] ->
            new_filter = add_expression_level_boolean_filter(filter, resource, key, value)

            if Ash.data_layer(resource).can?({:filter, key}) do
              new_filter
            else
              add_error(new_filter, "data layer does not support #{inspect(key)} filters")
            end

          attr = Ash.attribute(resource, key) ->
            add_attribute_filter(filter, attr, value)

          rel = Ash.relationship(resource, key) ->
            add_relationship_filter(filter, rel, value)

          true ->
            add_error(
              filter,
              "Attempted to filter on #{key} which is neither a relationship, nor a field of #{
                inspect(resource)
              }"
            )
        end
    end)
  end

  defp add_expression_level_boolean_filter(filter, resource, :not, expression) do
    Map.update!(filter, :not, fn
      nil ->
        parse(resource, expression, filter.api)

      not_filter ->
        do_parse(expression, not_filter)
    end)
  end

  defp add_expression_level_boolean_filter(filter, resource, key, {left, right}) do
    add_expression_level_boolean_filter(filter, resource, key, [left, right])
  end

  defp add_expression_level_boolean_filter(filter, _resource, :and, expressions) do
    Enum.reduce(expressions, filter, fn expression, filter ->
      do_parse(expression, filter)
    end)
  end

  defp add_expression_level_boolean_filter(filter, resource, :or, expressions) do
    Enum.reduce(expressions, filter, fn expression, filter ->
      parsed_expression = parse(resource, expression, filter.api)

      filter
      |> Map.update!(:ors, fn ors -> [parsed_expression | ors || []] end)
      |> add_error(parsed_expression.errors)
      |> add_request(parsed_expression.requests)
    end)
  end

  defp add_attribute_filter(filter, attr, value) do
    if Keyword.keyword?(value) do
      Enum.reduce(value, filter, fn
        {predicate_name, value}, filter ->
          do_add_attribute_filter(filter, attr, predicate_name, value)
      end)
    else
      add_attribute_filter(filter, attr, eq: value)
    end
  end

  defp do_add_attribute_filter(
         %{attributes: attributes, resource: resource} = filter,
         %{type: attr_type, name: attr_name},
         predicate_name,
         value
       ) do
    case parse_predicate(resource, predicate_name, attr_type, value) do
      {:ok, predicate} ->
        new_attributes =
          Map.update(
            attributes,
            attr_name,
            predicate,
            &Merge.merge(&1, predicate)
          )

        %{filter | attributes: new_attributes}

      {:error, error} ->
        add_error(filter, error)
    end
  end

  def parse_predicates(resource, keyword, attr_type) do
    Enum.reduce(keyword, {:ok, nil}, fn {predicate_name, value}, {:ok, existing_predicate} ->
      case parse_predicate(resource, predicate_name, attr_type, value) do
        {:ok, predicate} ->
          if existing_predicate do
            {:ok, Merge.merge(existing_predicate, predicate)}
          else
            {:ok, predicate}
          end

        {:error, error} ->
          {:error, error}
      end
    end)
  end

  defp parse_predicate(resource, predicate_name, attr_type, value) do
    data_layer = Ash.data_layer(resource)

    with {:predicate_type, {:ok, predicate_type}} <-
           {:predicate_type, Map.fetch(@predicates, predicate_name)},
         {:type_can?, _, true} <-
           {:type_can?, predicate_name,
            Ash.Type.supports_filter?(attr_type, predicate_name, data_layer)},
         {:data_layer_can?, _, true} <-
           {:data_layer_can?, predicate_name, data_layer.can?({:filter, predicate_name})},
         {:predicate, {:ok, predicate}} <-
           {:predicate, predicate_type.new(resource, attr_type, value)} do
      {:ok, predicate}
    else
      {:predicate_type, :error} ->
        {:error, "No such filter type #{predicate_name}"}

      {:predicate, {:error, error}} ->
        {:error, error}

      {:type_can?, predicate_name, false} ->
        {:error,
         "Cannot use filter type #{inspect(predicate_name)} on type #{inspect(attr_type)}."}

      {:data_layer_can?, predicate_name, false} ->
        {:error, "data layer not capable of provided filter: #{predicate_name}"}
    end
  end

  defp add_relationship_filter(
         %{relationships: relationships} = filter,
         %{destination: destination, name: name} = relationship,
         value
       ) do
    case parse_relationship_filter(value, relationship) do
      {:ok, provided_filter} ->
        related_filter = parse(destination, provided_filter, filter.api, [name | filter.path])

        new_relationships =
          Map.update(relationships, name, related_filter, &Merge.merge(&1, related_filter))

        filter
        |> Map.put(:relationships, new_relationships)
        |> add_relationship_compatibility_error(relationship)
        |> add_error(related_filter.errors)
        |> add_request(related_filter.requests)

      {:error, error} ->
        add_error(filter, error)
    end
  end

  defp parse_relationship_filter(value, %{destination: destination} = relationship) do
    cond do
      match?(%^destination{}, value) ->
        Ash.Actions.PrimaryKeyHelpers.value_to_primary_key_filter(destination, value)

      is_map(value) ->
        {:ok, Map.to_list(value)}

      Keyword.keyword?(value) ->
        {:ok, value}

      is_list(value) ->
        Enum.reduce_while(value, {:ok, []}, fn item, items ->
          case parse_relationship_filter(item, relationship) do
            {:ok, item_filter} -> {:cont, {:ok, [item_filter | items]}}
            {:error, error} -> {:halt, {:error, error}}
          end
        end)

      true ->
        Ash.Actions.PrimaryKeyHelpers.value_to_primary_key_filter(destination, value)
    end
  end

  defp add_relationship_compatibility_error(%{resource: resource} = filter, %{
         cardinality: cardinality,
         destination: destination,
         name: name
       }) do
    data_layer = Ash.data_layer(resource)

    cond do
      not data_layer.can?({:filter_related, cardinality}) ->
        add_error(
          filter,
          "Cannot filter on relationship #{name}: #{inspect(data_layer)} does not support it."
        )

      not (Ash.data_layer(destination) == data_layer) ->
        add_error(
          filter,
          "Cannot filter on related entites unless they share a data layer, for now."
        )

      true ->
        filter
    end
  end

  defp add_request(filter, requests)
       when is_list(requests),
       do: %{filter | requests: filter.requests ++ requests}

  defp add_request(%{requests: requests} = filter, request),
    do: %{filter | requests: [request | requests]}

  defp add_error(%{errors: errors} = filter, errors) when is_list(errors),
    do: %{filter | errors: filter.errors ++ errors}

  defp add_error(%{errors: errors} = filter, error), do: %{filter | errors: [error | errors]}
end
