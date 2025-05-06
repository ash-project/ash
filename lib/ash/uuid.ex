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

  @doc "Generates a new uuid"
  @spec generate() :: Ecto.UUID.t()
  def generate do
    Ecto.UUID.generate()
  end
end
