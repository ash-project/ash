defmodule Ash.Type.File.Path do
  @moduledoc false

  @behaviour Ash.Type.File.Implementation

  @impl Ash.Type.File.Implementation
  def path(path), do: {:ok, path}

  @impl Ash.Type.File.Implementation
  # sobelow_skip ["Traversal.FileModule"]
  def open(path, modes), do: File.open(path, modes)
end
