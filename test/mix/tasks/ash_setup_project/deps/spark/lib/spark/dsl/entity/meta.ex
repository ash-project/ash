# SPDX-FileCopyrightText: 2022 spark contributors <https://github.com/ash-project/spark/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Spark.Dsl.Entity.Meta do
  @moduledoc false

  defstruct [
    :anno,
    properties_anno: %{}
  ]
end
