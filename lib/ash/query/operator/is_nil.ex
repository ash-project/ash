defmodule Ash.Query.Operator.IsNil do
  @moduledoc """
  left is_nil true/false

  This predicate matches if the left is nil when the right is `true` or if the
  left is not nil when the right is `false`
  """
  use Ash.Query.Operator, operator: :is_nil

  def new(%Ref{} = left, right) when is_boolean(right) do
    {:ok, left, right}
  end

  def new(%Ref{}, right) do
    {:error, "#{inspect(right)} is not a valid operand for is_nil"}
  end

  def new(value, nil?), do: {:known, is_nil(value) == nil?}

  def match?(%{left: left, right: is_nil?}) do
    is_nil(left) == is_nil?
  end

  def to_string(%{left: left, right: right}, opts) do
    import Inspect.Algebra

    text =
      if right do
        " is nil"
      else
        " is not nil"
      end

    concat([
      to_doc(left, opts),
      text
    ])
  end

  def compare(%__MODULE__{left: %Ref{} = same_ref, right: true}, %Ash.Query.Operator.Eq{
        left: %Ref{} = same_ref,
        right: nil
      }) do
    :mutually_inclusive
  end

  def compare(%__MODULE__{left: %Ref{} = same_ref, right: false}, %Ash.Query.Operator.Eq{
        left: %Ref{} = same_ref,
        right: nil
      }) do
    :mutually_exclusive
  end

  def compare(%__MODULE__{left: %Ref{} = same_ref, right: false}, %Ash.Query.Operator.Eq{
        left: %Ref{} = same_ref
      }) do
    :right_includes_left
  end

  def compare(%__MODULE__{left: %Ref{} = same_ref}, %__MODULE__{left: %Ref{} = same_ref}) do
    :mutually_exclusive
  end

  def compare(_, _), do: :unknown
end
