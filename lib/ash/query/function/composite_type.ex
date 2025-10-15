# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Function.CompositeType do
  @moduledoc """
  Constructs a composite type in a way that is natively understood by the data layer

  To do this, provide a tuple matching the format expected by the type in question.
  Check that type's documentation for this information.
  """

  use Ash.Query.Function, name: :composite_type, eager_evaluate?: false

  def args, do: [[:any, :any], [:any, :any, :any]]

  def returns, do: [:any]

  def new([val, type]) do
    new([val, type, []])
  end

  def new([val, type, constraints]) do
    {:ok, %__MODULE__{arguments: [val, type, constraints]}}
  end

  def evaluate(%{arguments: [tuple, type]} = expr) do
    evaluate(%{expr | arguments: [tuple, type, []]})
  end

  def evaluate(%{arguments: [term, type, constraints]}) do
    case Ash.Type.cast_input(type, term, constraints) do
      {:ok, value} ->
        {:known, value}

      {:error, error} ->
        {:error, error}
    end
  end

  def can_return_nil?(%{arguments: [value | _]}) do
    Ash.Expr.can_return_nil?(value)
  end
end
