# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.EmbeddedArray.Domain do
  @moduledoc false
  use Ash.Domain

  resources do
    resource Ash.Test.EmbeddedArray.Estimate
  end
end
