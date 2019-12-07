defmodule Ash.Actions.Sort do
  def process(_resource, empty) when empty in [nil, []], do: {:ok, []}

  def process(resource, sort) when is_list(sort) do
    sort
    |> Enum.reduce({[], []}, fn
      {order, field}, {sorts, errors} when order in [:asc, :desc] ->
        attribute = Ash.attribute(resource, field)

        cond do
          !attribute ->
            {sorts, ["no such attribute: #{field}" | errors]}

          !Ash.Type.sortable?(attribute.type, Ash.data_layer(resource)) ->
            {sorts, ["Cannot sort on #{inspect(field)}"]}

          true ->
            {sorts ++ [{order, field}], errors}
        end

      sort, {sorts, errors} ->
        {sorts, ["invalid sort: #{inspect(sort)}" | errors]}
    end)
    |> case do
      {sorts, []} -> {:ok, sorts}
      {_, errors} -> {:error, errors}
    end
  end

  def process(_resource, _), do: {:error, "invalid sort"}
end
