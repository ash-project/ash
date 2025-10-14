# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Support.PolicyRbac.Domain do
  @moduledoc false
  use Ash.Domain

  resources do
    resource Ash.Test.Support.PolicyRbac.User
    resource Ash.Test.Support.PolicyRbac.Organization
    resource Ash.Test.Support.PolicyRbac.Membership
    resource Ash.Test.Support.PolicyRbac.File
  end
end
