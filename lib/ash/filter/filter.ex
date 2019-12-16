defmodule Ash.Filter do
  defstruct [
    :resource,
    :ors,
    attributes: %{},
    relationships: %{},
    errors: []
  ]

  alias Ash.Filter.Merge

  @type t :: %__MODULE__{
          resource: Ash.resource(),
          ors: %__MODULE__{} | nil,
          attributes: Keyword.t(),
          relationships: Keyword.t(),
          errors: list(String.t())
        }

  @predicates %{
    eq: Ash.Filter.Eq,
    in: Ash.Filter.In,
    and: Ash.Filter.And,
    or: Ash.Filter.Or
  }

  @spec parse(Ash.resource(), Keyword.t()) :: t()
  def parse(resource, filter) do
    do_parse(resource, filter, %Ash.Filter{resource: resource})
  end

  defp do_parse(resource, filter_statement, filter) do
    Enum.reduce(filter_statement, filter, fn
      {key, value}, filter ->
        cond do
          key == :or || key == :and ->
            add_expression_level_boolean_filter(filter, resource, key, value)

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

  defp add_expression_level_boolean_filter(filter, resource, key, {left, right}) do
    add_expression_level_boolean_filter(filter, resource, key, [left, right])
  end

  defp add_expression_level_boolean_filter(filter, resource, :and, expressions) do
    Enum.reduce(expressions, filter, fn expression, filter ->
      do_parse(resource, expression, filter)
    end)
  end

  defp add_expression_level_boolean_filter(filter, resource, :or, expressions) do
    Enum.reduce(expressions, filter, fn expression, filter ->
      parsed_expression = parse(resource, expression)

      filter
      |> Map.update!(:ors, fn ors -> [parsed_expression | ors] end)
      |> add_error(parsed_expression.errors)
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
    with {:predicate_type, {:ok, predicate_type}} <-
           {:predicate_type, Map.fetch(@predicates, predicate_name)},
         {:data_layer_can?, _, true} <-
           {:data_layer_can?, predicate_name,
            Ash.data_layer(resource).can?({:filter, predicate_name})},
         {:casted, {:ok, casted}} <- {:casted, Ash.Type.cast_input(attr_type, value)},
         {:predicate, {:ok, predicate}} = {:predicate, predicate_type.new(casted)} do
      new_attributes =
        Map.update(
          attributes,
          attr_name,
          predicate,
          &Merge.merge(&1, predicate)
        )

      %{filter | attributes: new_attributes}
    else
      {:predicate_type, :error} ->
        add_error(filter, "No such filter type #{predicate_name}")

      {:casted, _} ->
        add_error(filter, "Invalid value: #{inspect(value)} for #{inspect(attr_name)}")

      {:predicate, {:error, error}} ->
        add_error(filter, error)

      {:data_layer_can?, predicate_name, false} ->
        add_error(filter, "data layer not capable of provided filter: #{predicate_name}")
    end
  end

  def add_relationship_filter(
        %{relationships: relationships} = filter,
        %{destination: destination, name: name} = relationship,
        value
      ) do
    related_filter = parse(destination, value)
    filter_with_errors = Enum.reduce(related_filter.errors, filter, &add_error(&2, &1))

    new_relationships =
      Map.update(relationships, name, related_filter, &Merge.merge(&1, related_filter))

    filter_with_errors
    |> Map.put(:relationships, new_relationships)
    |> add_relationship_compatibility_error(relationship)
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

  defp add_error(%{errors: errors} = filter, errors) when is_list(errors),
    do: %{filter | errors: filter.errors ++ errors}

  defp add_error(%{errors: errors} = filter, error), do: %{filter | errors: [error | errors]}
end
