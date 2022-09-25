defmodule Ash.Test.Support.PolicyComplex.Api do
  @moduledoc false
  use Ash.Api

  resources do
    registry(Ash.Test.Support.PolicyComplex.Registry)
  end
end
