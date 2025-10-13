# SPDX-FileCopyrightText: 2020 Zach Daniel
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Function.Type do
  @moduledoc """
  Casts the value to a given type. Can also be used to provide type hints to data layers, where appropriate.
  """
  use Ash.Query.Function, name: :type, eager_evaluate?: false

  def args, do: [[:any, :any], [:any, :any, :any]]

  def returns, do: :unknown

  def new([val, type]) do
    new([val, type, []])
  end

  def new([val, type, constraints]) do
    {:ok, %__MODULE__{arguments: [val, type, constraints]}}
  end

  def can_return_nil?(%{arguments: [value | _]}) do
    Ash.Expr.can_return_nil?(value)
  end

  def evaluate(%{arguments: [val, type, constraints]}) do
    case Ash.Type.coerce(type, val, constraints) do
      {:ok, value} ->
        {:known, value}

      {:error, error} ->
        {:error, error}
    end
  end
end
