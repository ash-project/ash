defmodule Ash.Test.Support.PolicyRbac.Registry do
  @moduledoc false
  use Ash.Registry

  alias Ash.Test.Support.PolicyRbac, as: Rbac

  entries do
    entry(Rbac.User)
    entry(Rbac.Organization)
    entry(Rbac.Membership)
    entry(Rbac.File)
  end
end
