defmodule Ash.Test.Support.PolicyRbac.Domain do
  @moduledoc false
  use Ash.Domain, authorizers: [Ash.Policy.Authorizer]

  resources do
    resource Ash.Test.Support.PolicyRbac.User
    resource Ash.Test.Support.PolicyRbac.Organization
    resource Ash.Test.Support.PolicyRbac.Membership
    resource Ash.Test.Support.PolicyRbac.File
  end
end
