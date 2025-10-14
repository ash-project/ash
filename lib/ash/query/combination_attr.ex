# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.CombinationAttr do
  @moduledoc false
  defstruct [:name, :type, :constraints, load?: false]
end
