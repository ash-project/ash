if Code.ensure_loaded?(Plug.Upload) do
  defmodule Ash.Type.File.PlugUpload do
    @moduledoc false

    @behaviour Ash.Type.File.Implementation

    @impl Ash.Type.File.Implementation
    def path(%Plug.Upload{path: path}), do: {:ok, path}

    # sobelow_skip ["Traversal.FileModule"]
    @impl Ash.Type.File.Implementation
    def open(%Plug.Upload{path: path}, options), do: File.open(path, options)
  end

  defimpl Ash.Type.File.Source, for: Plug.Upload do
    def implementation(%Plug.Upload{}), do: {:ok, Ash.Type.File.PlugUpload}
  end
end
