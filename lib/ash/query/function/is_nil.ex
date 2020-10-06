defmodule Ash.Query.Function.IsNil do
  @moduledoc """
  true if the provided field is nil
  """
  use Ash.Query.Function, name: :is_nil

  def args, do: [:ref]

  def new([%Ref{} = ref]) do
    Ash.Query.Operator.new(Ash.Query.Operator.IsNil, ref, true)
  end

  def new(args) do
    {:error, "#{inspect(args)} are invalid for `is_nil`"}
  end

  # No need to define `match` because this function just turns into the operator
  # def match
end
