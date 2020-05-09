defmodule Ash.Filter.Or do
  defstruct [:left, :right]

  def new(resource, attr_name, attr_type, [first | [last | []]]) do
    with {:ok, first} <- Ash.Filter.parse_predicates(resource, first, attr_name, attr_type),
         {:ok, right} <- Ash.Filter.parse_predicates(resource, last, attr_name, attr_type) do
      {:ok, %__MODULE__{left: first, right: right}}
    end
  end

  def new(resource, attr_name, attr_type, [first | rest]) do
    case Ash.Filter.parse_predicates(resource, first, attr_name, attr_type) do
      {:ok, first} ->
        {:ok, %__MODULE__{left: first, right: new(resource, attr_name, attr_type, rest)}}

      {:error, error} ->
        {:error, error}
    end
  end

  def new(resource, attr_name, attr_type, {left, right}) do
    new(resource, attr_name, attr_type, [left, right])
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
end

defimpl Inspect, for: Ash.Filter.Or do
  import Inspect.Algebra

  def inspect(%{left: left, right: right}, opts) do
    concat([to_doc(left, opts), " or ", to_doc(right, opts)])
  end
end
