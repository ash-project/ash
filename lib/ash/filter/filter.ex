defmodule Ash.Filter do
  defstruct [
    :resource,
    :ors,
    :not,
    attributes: %{},
    relationships: %{},
    authorizations: [],
    errors: []
  ]

  alias Ash.Filter.Merge

  @type t :: %__MODULE__{
          resource: Ash.resource(),
          ors: list(%__MODULE__{} | nil),
          not: %__MODULE__{} | nil,
          attributes: Keyword.t(),
          relationships: Keyword.t(),
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

  @spec parse(Ash.resource(), Keyword.t(), rules :: list(term)) :: t()
  def parse(resource, filter, authorization_steps \\ nil) do
    authorization_steps =
      authorization_steps || Ash.primary_action(resource, :read).authorization_steps

    filter
    |> do_parse(%Ash.Filter{resource: resource})
    |> lift_ors()
    |> add_not_filter_info()
    |> add_authorization(
      Ash.Authorization.Request.new(
        resource: resource,
        authorization_steps: authorization_steps,
        filter: filter
      )
    )
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
        parse(resource, expression)

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
      parsed_expression = parse(resource, expression)

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
        related_filter = parse(destination, provided_filter)

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

  defp add_authorization(%{authorizations: authorizations} = filter, authorizations)
       when is_list(authorizations),
       do: %{filter | authorizations: filter.authorizations ++ authorizations}

  defp add_authorization(%{authorizations: authorizations} = filter, authorization),
    do: %{filter | authorizations: [authorization | authorizations]}

  defp add_error(%{errors: errors} = filter, errors) when is_list(errors),
    do: %{filter | errors: filter.errors ++ errors}

  defp add_error(%{errors: errors} = filter, error), do: %{filter | errors: [error | errors]}
end
