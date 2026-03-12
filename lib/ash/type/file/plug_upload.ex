# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

if Code.ensure_loaded?(Plug.Upload) do
  defmodule Ash.Type.File.PlugUpload do
    @moduledoc false

    @behaviour Ash.Type.File.Implementation

    @impl Ash.Type.File.Implementation
    def path(%Plug.Upload{path: path}), do: {:ok, path}

    # sobelow_skip ["Traversal.FileModule"]
    @impl Ash.Type.File.Implementation
    def open(%Plug.Upload{path: path}, options), do: File.open(path, options)

    @impl Ash.Type.File.Implementation
    def filename(%Plug.Upload{filename: filename}), do: {:ok, filename}

    @impl Ash.Type.File.Implementation
    def content_type(%Plug.Upload{content_type: content_type}), do: {:ok, content_type}
  end

  defimpl Ash.Type.File.Source, for: Plug.Upload do
    def implementation(%Plug.Upload{}), do: {:ok, Ash.Type.File.PlugUpload}
  end
end
