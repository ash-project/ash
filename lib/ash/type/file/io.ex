# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Type.File.IO do
  @moduledoc false

  @behaviour Ash.Type.File.Implementation

  @impl Ash.Type.File.Implementation
  def open(device, _modes), do: {:ok, device}
end
