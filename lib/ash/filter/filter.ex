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

  # THe story here:
  # we don't really need to fully simplify every value statement, e.g `in: [1, 2, 3]` -> `== 1 or == 2 or == 3`
  # We could instead just simplify *only as much as we need to*, for instance if the filter contains
  # `in: [1, 2, 3]` and `in: [2, 3, 4]`, we could translate the first to `in: [2, 3] or == 1` and the
  # second one to `in: [2, 3] or == 4`. We should then be able to go about expressing the fact that none
  # of `== 1` and `== 2` are mutually exclusive terms by exchanging them for `== 1 and != 2` and `== 2 and != 1`
  # respectively. This is the methodology behind translating a *value* based filter into a boolean expression.
  #
  # However for now for simplicity's sake, I'm turning all `in: [1, 2]` into `== 1 or == 2` and all `not_in: [1, 2]`
  # into `!= 1 and !=2` for the sole reason that its not worth figuring it out right now. Cosimplification is, at the
  # and of the day, really just an optimization to keep the expression simple. Its not so important with lists and equality
  # but when we add substring filters/greater than filters, we're going to need to improve this logic
  def cosimplify(left, right) do
    {new_left, new_right} = simplify_lists(left, right)

    express_mutual_exclusion(new_left, new_right)
  end

  defp simplify_lists(left, right) do
    values = get_all_values(left, get_all_values(right, %{}))

    substitutions =
      Enum.reduce(values, %{}, fn {key, values}, substitutions ->
        value_substitutions =
          Enum.reduce(values, %{}, fn value, substitutions ->
            case do_simplify_list(value) do
              {:ok, substitution} ->
                Map.put(substitutions, value, substitution)

              :error ->
                substitutions
            end
          end)

        Map.put(substitutions, key, value_substitutions)
      end)

    {replace_values(left, substitutions), replace_values(right, substitutions)}
  end

  defp do_simplify_list(%Ash.Filter.In{values: []}), do: :error

  defp do_simplify_list(%Ash.Filter.In{values: [value]}) do
    {:ok, %Ash.Filter.Eq{value: value}}
  end

  defp do_simplify_list(%Ash.Filter.In{values: [value | rest]}) do
    {:ok,
     Enum.reduce(rest, %Ash.Filter.Eq{value: value}, fn value, other_values ->
       Ash.Filter.Or.prebuilt_new(%Ash.Filter.Eq{value: value}, other_values)
     end)}
  end

  defp do_simplify_list(%Ash.Filter.NotIn{values: []}), do: :error

  defp do_simplify_list(%Ash.Filter.NotIn{values: [value]}) do
    {:ok, %Ash.Filter.NotEq{value: value}}
  end

  defp do_simplify_list(%Ash.Filter.NotIn{values: [value | rest]}) do
    {:ok,
     Enum.reduce(rest, %Ash.Filter.Eq{value: value}, fn value, other_values ->
       Ash.Filter.And.prebuilt_new(%Ash.Filter.NotEq{value: value}, other_values)
     end)}
  end

  defp do_simplify_list(_), do: :error

  defp express_mutual_exclusion(left, right) do
    values = get_all_values(left, get_all_values(right, %{}))

    substitutions =
      Enum.reduce(values, %{}, fn {key, values}, substitutions ->
        value_substitutions =
          Enum.reduce(values, %{}, fn value, substitutions ->
            case do_express_mutual_exclusion(value, values) do
              {:ok, substitution} ->
                Map.put(substitutions, value, substitution)

              :error ->
                substitutions
            end
          end)

        Map.put(substitutions, key, value_substitutions)
      end)

    {replace_values(left, substitutions), replace_values(right, substitutions)}
  end

  defp do_express_mutual_exclusion(%Ash.Filter.Eq{value: value} = eq_filter, values) do
    values
    |> Enum.filter(fn
      %Ash.Filter.Eq{value: other_value} -> value != other_value
      _ -> false
    end)
    |> case do
      [] ->
        :error

      [%{value: other_value}] ->
        {:ok, Ash.Filter.And.prebuilt_new(eq_filter, %Ash.Filter.NotEq{value: other_value})}

      values ->
        {:ok,
         Enum.reduce(values, eq_filter, fn %{value: other_value}, expr ->
           Ash.Filter.And.prebuilt_new(expr, %Ash.Filter.NotEq{value: other_value})
         end)}
    end
  end

  defp do_express_mutual_exclusion(_, _), do: :error

  defp get_all_values(filter, state) do
    state =
      filter.attributes
      # TODO
      |> Enum.reduce(state, fn {field, value}, state ->
        state
        |> Map.put_new([filter.path, field], [])
        |> Map.update!([filter.path, field], fn values ->
          value
          |> do_get_values()
          |> Enum.reduce(values, fn value, values ->
            if value in values do
              values
            else
              [value | values]
            end
          end)
        end)
      end)

    state =
      Enum.reduce(filter.relationships, state, fn {_, relationship_filter}, new_state ->
        get_all_values(relationship_filter, new_state)
      end)

    state =
      if filter.not do
        get_all_values(filter, state)
      else
        state
      end

    Enum.reduce(filter.ors, state, fn or_filter, new_state ->
      get_all_values(or_filter, new_state)
    end)
  end

  defp do_get_values(%struct{left: left, right: right})
       when struct in [Ash.Filter.And, Ash.Filter.Or] do
    do_get_values(left) ++ do_get_values(right)
  end

  defp do_get_values(other), do: [other]

  defp replace_values(filter, substitutions) do
    new_attrs =
      Enum.reduce(filter.attributes, %{}, fn {field, value}, attributes ->
        substitutions = Map.get(substitutions, [filter.path, field]) || %{}

        Map.put(attributes, field, do_replace_value(value, substitutions))
      end)

    new_relationships =
      Enum.reduce(filter.relationships, %{}, fn {relationship, related_filter}, relationships ->
        new_relationship_filter = replace_values(related_filter, substitutions)

        Map.put(relationships, relationship, new_relationship_filter)
      end)

    new_not =
      if filter.not do
        replace_values(filter, substitutions)
      else
        filter.not
      end

    new_ors =
      Enum.reduce(filter.ors, [], fn or_filter, ors ->
        new_or = replace_values(or_filter, substitutions)

        [new_or | ors]
      end)

    %{
      filter
      | attributes: new_attrs,
        relationships: new_relationships,
        not: new_not,
        ors: Enum.reverse(new_ors)
    }
  end

  defp do_replace_value(%struct{left: left, right: right} = compound, substitutions)
       when struct in [Ash.Filter.And, Ash.Filter.Or] do
    %{
      compound
      | left: do_replace_value(left, substitutions),
        right: do_replace_value(right, substitutions)
    }
  end

  defp do_replace_value(value, substitutions) do
    case Map.fetch(substitutions, value) do
      {:ok, new_value} ->
        new_value

      _ ->
        value
    end
  end

  @doc """
  Returns true if the second argument is a strict subset (always returns the same or less data) of the first
  """
  def strict_subset_of(nil, _), do: true

  def strict_subset_of(_, nil), do: false

  def strict_subset_of(%{resource: resource}, %{resource: other_resource})
      when resource != other_resource,
      do: false

  def strict_subset_of(filter, candidate) do
    if empty_filter?(filter) do
      true
    else
      if empty_filter?(candidate) do
        false
      else
        {filter, candidate} = cosimplify(filter, candidate)
        Ash.Authorization.SatSolver.strict_filter_subset(filter, candidate)
      end
    end
  end

  def strict_subset_of?(filter, candidate) do
    strict_subset_of(filter, candidate) == true
  end

  defp empty_filter?(filter) do
    filter.attributes == %{} and filter.relationships == %{} and filter.not == nil and
      filter.ors in [[], nil]
  end

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
    case parse_predicate(resource, predicate_name, attr_name, attr_type, value) do
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

  def parse_predicates(resource, keyword, attr_name, attr_type) do
    Enum.reduce(keyword, {:ok, nil}, fn {predicate_name, value}, {:ok, existing_predicate} ->
      case parse_predicate(resource, predicate_name, attr_name, attr_type, value) do
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

  defp parse_predicate(resource, predicate_name, attr_name, attr_type, value) do
    data_layer = Ash.data_layer(resource)

    with {:predicate_type, {:ok, predicate_type}} <-
           {:predicate_type, Map.fetch(@predicates, predicate_name)},
         {:type_can?, _, true} <-
           {:type_can?, predicate_name,
            Ash.Type.supports_filter?(attr_type, predicate_name, data_layer)},
         {:data_layer_can?, _, true} <-
           {:data_layer_can?, predicate_name, data_layer.can?({:filter, predicate_name})},
         {:predicate, _, {:ok, predicate}} <-
           {:predicate, attr_name, predicate_type.new(resource, attr_name, attr_type, value)} do
      {:ok, predicate}
    else
      {:predicate_type, :error} ->
        {:error, "No such filter type #{predicate_name}"}

      {:predicate, attr_name, {:error, error}} ->
        {:error, Map.put(error, :field, attr_name)}

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

defimpl Inspect, for: Ash.Filter do
  import Inspect.Algebra
  import Ash.Filter.InspectHelpers

  defguardp is_empty(val)
            when is_nil(val) or (is_map(val) and map_size(val) == 0) or length(val) == 0

  def inspect(%Ash.Filter{not: not_filter} = filter, opts) when not is_nil(not_filter) do
    impossible = if Map.fetch!(filter, :impossible?), do: "X", else: ""

    if root?(opts) do
      concat([
        "#Filter<#{impossible} not (",
        to_doc(not_filter, make_non_root(opts)),
        ") and ",
        to_doc(%{filter | not: nil}, make_non_root(opts)),
        ">"
      ])
    else
      concat([
        "not (",
        to_doc(not_filter, make_non_root(opts)),
        ") and ",
        to_doc(%{filter | not: nil}, make_non_root(opts))
      ])
    end
  end

  def inspect(
        %Ash.Filter{ors: ors, relationships: relationships, attributes: attributes} = filter,
        opts
      )
      when is_empty(ors) and is_empty(relationships) and is_empty(attributes) do
    impossible = if Map.fetch!(filter, :impossible?), do: "X", else: ""

    if root?(opts) do
      concat(["#Filter<#{impossible}", to_doc(nil, opts), ">"])
    else
      concat([impossible])
    end
  end

  def inspect(filter, opts) do
    impossible = if Map.fetch!(filter, :impossible?), do: "X", else: ""

    rels = parse_relationships(filter, opts)
    attrs = parse_attributes(filter, opts)

    and_container =
      case attrs ++ rels do
        [] ->
          empty()

        and_clauses ->
          Inspect.Algebra.container_doc("(", and_clauses, ")", opts, fn term, _ -> term end,
            break: :flex,
            separator: " and"
          )
      end

    all_container =
      case Map.get(filter, :ors) do
        nil ->
          and_container

        [] ->
          and_container

        ors ->
          inspected_ors = Enum.map(ors, fn filter -> to_doc(filter, make_non_root(opts)) end)

          Inspect.Algebra.container_doc(
            "",
            [and_container | inspected_ors],
            "",
            opts,
            fn term, _ -> term end,
            break: :strict,
            separator: " or "
          )
      end

    if root?(opts) do
      concat(["#Filter<#{impossible}", all_container, ">"])
    else
      concat([impossible, all_container])
    end
  end

  defp parse_relationships(%Ash.Filter{relationships: %{}}, _opts), do: []

  defp parse_relationships(filter, opts) do
    filter
    |> Map.fetch!(:relationships)
    |> Enum.map(fn {key, value} -> to_doc(value, add_to_path(opts, key)) end)
  end

  defp parse_attributes(%Ash.Filter{attributes: %{}}, _opts), do: []

  defp parse_attributes(filter, opts) do
    filter
    |> Map.fetch!(:attributes)
    |> Enum.map(fn {key, value} -> to_doc(value, put_attr(opts, key)) end)
  end
end
