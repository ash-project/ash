# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Forbidden.MustPassStrictCheck do
  @moduledoc "Used when unreachable code/conditions are reached in the framework"
  use Splode.Error, fields: [], class: :forbidden

  def message(_) do
    "The request was required to pass strict check, but it did not"
  end
end
