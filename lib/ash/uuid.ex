# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.UUID do
  @moduledoc "Helpers for working with UUIDs"

  @typedoc """
  A hex-encoded UUID string.
  """
  @type t :: Ecto.UUID.t()

  @typedoc """
  A raw binary representation of a UUID.
  """
  @type raw :: Ecto.UUID.raw()

  @doc """
  Generates a new uuid.
  """
  @spec generate(Keyword.t()) :: Ecto.UUID.t()
  def generate(opts \\ []) do
    Ecto.UUID.generate(opts)
  end
end
