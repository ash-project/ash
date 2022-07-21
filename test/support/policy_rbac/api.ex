defmodule Ash.Test.Support.PolicyRbac.Api do
  @moduledoc false
  use Ash.Api

  resources do
    registry(Ash.Test.Support.PolicyRbac.Registry)
  end
end
