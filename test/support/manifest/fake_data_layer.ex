# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.FakeDataLayer do
  @moduledoc """
  A minimal data layer used only by `CapabilitiesBuilder` tests to verify
  that data-layer functions are collected into the manifest's filter catalog.

  It only implements `functions/1`; manifest generation never invokes the
  rest of the data-layer behaviour, so the rest is intentionally omitted.
  """

  @spec functions(Ash.Resource.t()) :: [module()]
  def functions(_resource), do: [Ash.Test.Manifest.FakeDataLayerFunction]
end
