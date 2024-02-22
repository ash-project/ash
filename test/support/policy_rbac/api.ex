defmodule Ash.Test.Support.PolicyRbac.Api do
  @moduledoc false
  use Ash.Api

  resources do
    resource Ash.Test.Support.PolicyRbac.User
    resource Ash.Test.Support.PolicyRbac.Organization
    resource Ash.Test.Support.PolicyRbac.Membership
    resource Ash.Test.Support.PolicyRbac.File
  end
end
