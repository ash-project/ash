defmodule Ash.Filter do
  defstruct [
    :api,
    :resource,
    :ors,
    :not,
    attributes: %{},
    relationships: %{},
    authorizations: [],
    path: [],
    errors: []
  ]

  alias Ash.Filter.Merge

  @type t :: %__MODULE__{
          api: Ash.api(),
          resource: Ash.resource(),
          ors: list(%__MODULE__{} | nil),
          not: %__MODULE__{} | nil,
          attributes: Keyword.t(),
          relationships: Keyword.t(),
          path: list(atom),
          errors: list(String.t()),
          authorizations: list(Ash.Authorization.Request.t())
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
  # The `api` argument is here primarily because the authorization requests
  # need to have the `api`.
  # TODO: Remove this by making it so that authorization steps are generated
  # *after* the filter is generated. We can traverse all the relationships
  # and figure out what authorizations are required. That makes this much nicer.
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
      authorization_request =
        Ash.Authorization.Request.new(
          resource: resource,
          api: api,
          authorization_steps: Ash.primary_action(resource, :read).authorization_steps,
          filter: parsed_filter,
          fetcher: fn ->
            query = Ash.DataLayer.resource_to_query(resource)

            case Ash.DataLayer.filter(query, parsed_filter, resource) do
              {:ok, filtered_query} ->
                Ash.DataLayer.run_query(filtered_query, resource)

              {:error, error} ->
                {:error, error}
            end
          end,
          action_type: :read,
          bypass_strict_access?: bypass_strict_access?(parsed_filter),
          relationship: path,
          source: source
        )

      add_authorization(
        parsed_filter,
        authorization_request
      )
    end
  end

  @doc """
  Returns true if the second argument is a strict subset (always returns the same or less data) of the first
  """
  def strict_subset_of?(nil, nil), do: true

  def strict_subset_of?(_, nil), do: false

  def strict_subset_of?(filter, candidate) do
    unless filter.ors in [[], nil], do: raise("Can't do ors contains yet")
    unless filter.not in [[], nil], do: raise("Can't do not contains yet")
    unless candidate.ors in [[], nil], do: raise("Can't do ors contains yet")
    unless candidate.not in [[], nil], do: raise("Can't do not contains yet")

    attributes_contained? =
      Enum.any?(filter.attributes, fn {attr, predicate} ->
        contains_attribute?(candidate, attr, predicate)
      end)

    relationships_contained? =
      Enum.any?(filter.relationships, fn {relationship, relationship_filter} ->
        contains_relationship?(candidate, relationship, relationship_filter)
      end)

    # TODO: put these behind functions to optimize them.
    attributes_contained? or relationships_contained?
  end

  defp bypass_strict_access?(%{ors: ors, not: not_filter} = filter) when is_nil(not_filter) do
    pkey_fields = Ash.primary_key(filter.resource)

    Enum.all?(pkey_fields, &is_filtering_on_known_value_for_attribute(filter, &1)) &&
      Enum.all?(ors || [], fn filter ->
        Enum.all?(pkey_fields, &is_filtering_on_known_value_for_attribute(filter, &1))
      end)
  end

  defp bypass_strict_access?(_), do: false

  defp is_filtering_on_known_value_for_attribute(filter, field) do
    case Map.fetch(filter.attributes, field) do
      {:ok, attribute_filter} ->
        known_value_filter?(attribute_filter)

      :error ->
        false
    end
  end

  defp known_value_filter?(%Ash.Filter.And{left: left, right: right}) do
    known_value_filter?(left) or known_value_filter?(right)
  end

  defp known_value_filter?(%Ash.Filter.Or{left: left, right: right}) do
    known_value_filter?(left) and known_value_filter?(right)
  end

  defp known_value_filter?(%Ash.Filter.In{values: values}) when values != [] do
    true
  end

  defp known_value_filter?(%Ash.Filter.Eq{value: value}) when not is_nil(value) do
    true
  end

  defp known_value_filter?(_), do: false

  defp contains_relationship?(filter, relationship, candidate_relationship_filter) do
    case filter.relationships do
      %{^relationship => relationship_filter} ->
        strict_subset_of?(relationship_filter, candidate_relationship_filter)

      _ ->
        false
    end
  end

  defp contains_attribute?(filter, attr, candidate_predicate) do
    case filter.attributes do
      %{^attr => predicate} ->
        attribute = Ash.attribute(filter.resource, attr)

        predicate_strict_subset_of?(attribute, predicate, candidate_predicate)

      _ ->
        false
    end
  end

  defp add_not_filter_info(filter) do
    case filter.not do
      nil ->
        filter

      not_filter ->
        filter
        |> add_authorization(not_filter.authorizations)
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
         authorizations: authorizations
       })
       when attrs == %{} and rels == %{} do
    or_filter
    |> Map.put(:ors, rest)
    |> add_error(errors)
    |> add_authorization(authorizations)
    |> lift_ors()
  end

  defp lift_ors(filter), do: filter

  defp do_parse(filter_statement, %{resource: resource} = filter) do
    Enum.reduce(filter_statement, filter, fn
      {key, value}, filter ->
        cond do
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
      |> add_authorization(parsed_expression.authorizations)
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
    provided_filter =
      if is_list(value) or is_map(value) do
        {:ok, value}
      else
        Ash.Actions.PrimaryKeyHelpers.value_to_primary_key_filter(destination, value)
      end

    case provided_filter do
      {:ok, provided_filter} ->
        related_filter = parse(destination, provided_filter, filter.api, [name | filter.path])

        new_relationships =
          Map.update(relationships, name, related_filter, &Merge.merge(&1, related_filter))

        filter
        |> Map.put(:relationships, new_relationships)
        |> add_relationship_compatibility_error(relationship)
        |> add_error(related_filter.errors)
        |> add_authorization(related_filter.authorizations)

      {:error, error} ->
        add_error(filter, error)
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

  defp add_authorization(filter, authorizations)
       when is_list(authorizations),
       do: %{filter | authorizations: filter.authorizations ++ authorizations}

  defp add_authorization(%{authorizations: authorizations} = filter, authorization),
    do: %{filter | authorizations: [authorization | authorizations]}

  defp add_error(%{errors: errors} = filter, errors) when is_list(errors),
    do: %{filter | errors: filter.errors ++ errors}

  defp add_error(%{errors: errors} = filter, error), do: %{filter | errors: [error | errors]}
end
