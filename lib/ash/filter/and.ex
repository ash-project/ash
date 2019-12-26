defmodule Ash.Filter.And do
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
    %__MODULE__{left: left, right: right}
  end

  def contains?(%{left: left, right: right}, predicate) do
    Ash.Filter.predicate_contains?(left, predicate) or
      Ash.Filter.predicate_contains?(right, predicate)
  end

  def contains?(_, _), do: false
end
