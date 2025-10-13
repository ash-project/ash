# SPDX-FileCopyrightText: 2020 Zach Daniel
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.CombinationAttr do
  @moduledoc false
  defstruct [:name, :type, :constraints, load?: false]
end
