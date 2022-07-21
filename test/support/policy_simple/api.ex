defmodule Ash.Test.Support.PolicySimple.Api do
  @moduledoc false
  use Ash.Api

  resources do
    registry(Ash.Test.Support.PolicySimple.Registry)
  end
end
