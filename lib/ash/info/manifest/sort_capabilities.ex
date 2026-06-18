# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Info.Manifest.SortCapabilities do
  @moduledoc """
  Top-level sort vocabulary for a manifest. The directions list is fixed —
  Ash's sort directions don't vary per resource or app config.
  """

  @type direction ::
          :asc | :desc | :asc_nils_first | :asc_nils_last | :desc_nils_first | :desc_nils_last

  @type t :: %__MODULE__{directions: [direction()], custom: map()}

  defstruct directions: [
              :asc,
              :desc,
              :asc_nils_first,
              :asc_nils_last,
              :desc_nils_first,
              :desc_nils_last
            ],
            custom: %{}
end
