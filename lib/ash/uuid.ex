defmodule Ash.UUID do
  @moduledoc "Helpers for working with UUIDs"

  @doc "Generates a new uuid"
  def generate do
    Ecto.UUID.generate()
  end
end
