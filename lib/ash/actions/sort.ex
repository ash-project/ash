defmodule Ash.Actions.Sort do
  alias Ash.Error.Sort.{InvalidSortOrder, NoSuchField, UnsortableField}

  def process(_resource, empty) when empty in [nil, []], do: {:ok, []}

  def process(resource, sort) when is_list(sort) do
    sort
    |> Enum.reduce({[], []}, fn
      {field, order}, {sorts, errors} when order in [:asc, :desc] ->
        attribute = Ash.attribute(resource, field)

        cond do
          !attribute ->
            {sorts, [NoSuchField.exception(field: field) | errors]}

          !Ash.Type.sortable?(attribute.type, Ash.data_layer(resource)) ->
            {sorts,
             [
               UnsortableField.exception(field: field)
               | errors
             ]}

          true ->
            {sorts ++ [{field, order}], errors}
        end

      {_, order}, {sorts, errors} ->
        {sorts, [InvalidSortOrder.exception(order: order) | errors]}
    end)
    |> case do
      {sorts, []} -> {:ok, sorts}
      {_, errors} -> {:error, errors}
    end
  end
end
