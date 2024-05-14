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
    if right == false and not Ash.Expr.can_return_nil?(left) do
      {:known, false}
    else
      super(left, right)
    end
  end

  @impl Ash.Query.Operator
  def evaluate_nil_inputs?, do: true

  @impl Ash.Query.Operator
  def evaluate(%{right: nil}), do: {:known, nil}

  def evaluate(%{left: left, right: is_nil?}) do
    {:known, is_nil(left) == is_nil?}
  end

  def to_string(%{left: left, right: right}, opts) do
    import Inspect.Algebra

    cond do
      right == true ->
        concat([
          "is_nil(",
          to_doc(left, opts),
          ")"
        ])

      right == false ->
        concat([
          "not(is_nil(",
          to_doc(left, opts),
          ")"
        ])

      true ->
        concat([
          " is_nil(",
          to_doc(left, opts),
          ") == ",
          to_doc(right, opts)
        ])
    end
  end

  def can_return_nil?(%{right: right}), do: Ash.Expr.can_return_nil?(right)

  @impl Ash.Filter.Predicate
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
