defmodule Ash.Policy.Test.Rbac.Api do
  @moduledoc false
  use Ash.Api

  resources do
    registry(Ash.Policy.Test.Rbac.Registry)
  end
end
