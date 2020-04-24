defmodule Ash.Filter.Or do
  defstruct [:left, :right]

  def new(resource, attr_type, [first | [last | []]]) do
    with {:ok, first} <- Ash.Filter.parse_predicates(resource, first, attr_type),
         {:ok, right} <- Ash.Filter.parse_predicates(resource, last, attr_type) do
      {:ok, %__MODULE__{left: first, right: right}}
    end
  end

  def new(resource, attr_type, [first | rest]) do
    case Ash.Filter.parse_predicates(resource, first, attr_type) do
      {:ok, first} ->
        {:ok, %__MODULE__{left: first, right: new(resource, attr_type, rest)}}

      {:error, error} ->
        {:error, error}
    end
  end

  def new(resource, attr_type, {left, right}) do
    new(resource, attr_type, [left, right])
  end

  def prebuilt_new(left, right) do
    # TODO: This should probably get richer. For instance, if right is a strict subset of left,
    # maybe we should strip it?
    if left == right do
      left
    else
      %__MODULE__{left: left, right: right}
    end
  end

  def strict_subset_of?(attr, %{left: left, right: right}, predicate) do
    Ash.Filter.predicate_strict_subset_of?(attr, left, predicate) and
      Ash.Filter.predicate_strict_subset_of?(attr, right, predicate)
  end

  def strict_subset_of?(_, _, _), do: false
end
