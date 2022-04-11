defmodule Ash.Query.Operator.IsNil do
  @moduledoc """
  left is_nil true/false

  This predicate matches if the left is nil when the right is `true` or if the
  left is not nil when the right is `false`
  """
  use Ash.Query.Operator,
    operator: :is_nil,
    predicate?: true,
    types: [[:any, :boolean]]

  def new(nil, true), do: {:ok, true}
  def new(nil, false), do: {:ok, false}

  def new(left, right) do
    if Ash.Query.is_expr?(left) do
      super(left, right)
    else
      cond do
        right == true ->
          {:ok, false}

        right == false ->
          {:ok, true}

        true ->
          super(left, right)
      end
    end
  end

  def evaluate(%{right: nil}), do: {:ok, nil}

  def evaluate(%{left: left, right: is_nil?}) do
    {:known, is_nil(left) == is_nil?}
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
    :mutually_exclusive_and_collectively_exhaustive
  end

  def compare(%__MODULE__{left: %Ref{} = same_ref, right: false}, %Ash.Query.Operator.Eq{
        left: %Ref{} = same_ref,
        right: %Ref{}
      }) do
    :unknown
  end

  def compare(%__MODULE__{left: %Ref{} = same_ref, right: false}, %Ash.Query.Operator.Eq{
        left: %Ref{} = same_ref
      }) do
    :right_includes_left
  end

  def compare(%__MODULE__{left: %Ref{} = same_ref, right: true}, %__MODULE__{
        left: %Ref{} = same_ref,
        right: false
      }) do
    :mutually_exclusive_and_collectively_exhaustive
  end

  def compare(%__MODULE__{left: %Ref{} = same_ref, right: false}, %__MODULE__{
        left: %Ref{} = same_ref,
        right: true
      }) do
    :mutually_exclusive_and_collectively_exhaustive
  end

  def compare(%__MODULE__{left: %Ref{} = same_ref, right: right}, %__MODULE__{
        left: %Ref{} = same_ref,
        right: right
      })
      when is_boolean(right) do
    :mutually_inclusive
  end

  def compare(_left, _right) do
    :unknown
  end
end
