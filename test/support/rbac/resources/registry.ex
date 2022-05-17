defmodule Ash.Policy.Test.Rbac.Registry do
  @moduledoc false
  use Ash.Registry

  alias Ash.Policy.Test.Rbac

  entries do
    entry(Rbac.User)
    entry(Rbac.Organization)
    entry(Rbac.Membership)
    entry(Rbac.File)
  end
end
