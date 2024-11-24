defmodule Ash.UUID do
  @moduledoc "Helpers for working with UUIDs"

  @doc "Generates a new uuid"
  @spec generate() :: Ecto.UUID.t()
  def generate do
    Ecto.UUID.generate()
  end
end
