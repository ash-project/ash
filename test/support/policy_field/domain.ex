# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Support.PolicyField.Domain do
  @moduledoc false
  use Ash.Domain

  resources do
    allow_unregistered? true
  end
end
