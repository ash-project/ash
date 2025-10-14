# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Vector do
  @moduledoc """
  A vector struct for Ash.

  Implementation based off of https://github.com/pgvector/pgvector-elixir/blob/v0.2.0/lib/pgvector.ex
  """

  defstruct [:data, :dimensions]

  @type t :: %__MODULE__{
          data: binary(),
          dimensions: pos_integer()
        }

  @doc """
  Creates a new vector from a list or tensor
  """
  @spec new(t() | binary() | list(integer)) :: {:ok, t()} | {:error, :invalid_vector}
  def new(%__MODULE__{} = vector), do: {:ok, vector}

  def new(binary) when is_binary(binary) do
    new(:erlang.binary_to_list(binary))
  end

  def new(list) when is_list(list) do
    dim = list |> length()
    bin = for v <- list, into: "", do: <<v::float-32>>

    {:ok, %Ash.Vector{data: <<dim::unsigned-16, 0::unsigned-16>> <> bin, dimensions: dim}}
  rescue
    _ ->
      {:error, :invalid_vector}
  end

  @doc """
  Creates a new vector from its binary representation
  """
  @spec from_binary(binary()) :: t()
  def from_binary(binary) when is_binary(binary) do
    <<dim::unsigned-16>> <> _ = binary
    %Ash.Vector{data: binary, dimensions: dim}
  end

  @doc """
  Converts the vector to its binary representation
  """
  @spec to_binary(t()) :: binary()
  def to_binary(vector) when is_struct(vector, Ash.Vector) do
    vector.data
  end

  @doc """
  Converts the vector to a list
  """
  @spec to_list(t()) :: list()
  def to_list(vector) when is_struct(vector, Ash.Vector) do
    <<dim::unsigned-16, 0::unsigned-16, bin::binary-size(dim)-unit(32)>> = vector.data
    for <<v::float-32 <- bin>>, do: v
  end
end

defimpl Inspect, for: Ash.Vector do
  import Inspect.Algebra

  @doc false
  def inspect(vec, opts) do
    concat(["Ash.Vector.new(", Inspect.List.inspect(Ash.Vector.to_list(vec), opts), ")"])
  end
end
