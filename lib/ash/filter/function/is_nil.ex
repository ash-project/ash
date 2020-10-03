defmodule Ash.Filter.Function.IsNil do
  use Ash.Filter.Function, name: :is_nil

  def args, do: [:ref]

  def new([%Ref{} = ref]) do
    Ash.Filter.Operator.new(Ash.Filter.Operator.IsNil, ref, true)
  end

  def new(args) do
    {:error, "#{inspect(args)} are invalid for `is_nil`"}
  end
end
