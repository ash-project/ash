defmodule Ash.Filter.And do
  @moduledoc false
  defstruct [:left, :right]

  def new(resource, attr_name, attr_type, [first | [last | []]]) do
    with {:ok, first} <- Ash.Filter.parse_predicates(resource, first, attr_name, attr_type),
         {:ok, right} <- Ash.Filter.parse_predicates(resource, last, attr_name, attr_type) do
      if first == right do
        {:ok, first}
      else
        {:ok, %__MODULE__{left: first, right: right}}
      end
    end
  end

  def new(resource, attr_name, attr_type, [first | rest]) do
    case Ash.Filter.parse_predicates(resource, first, attr_name, attr_type) do
      {:ok, first} ->
        case new(resource, attr_name, attr_type, rest) do
          {:ok, right} when right == first ->
            {:ok, right}

          {:ok, right} ->
            {:ok, %__MODULE__{left: first, right: right}}

          {:error, error} ->
            {:error, error}
        end

      {:error, error} ->
        {:error, error}
    end
  end

  def new(resource, attr_name, attr_type, {left, right}) do
    new(resource, attr_name, attr_type, [left, right])
  end

  def prebuilt_new(left, right) do
    if left == right do
      left
    else
      %__MODULE__{left: left, right: right}
    end
  end
end

defimpl Inspect, for: Ash.Filter.And do
  import Inspect.Algebra

  def inspect(%{left: left, right: right}, opts) do
    concat([to_doc(left, opts), " and ", to_doc(right, opts)])
  end
end
