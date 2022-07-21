defmodule Ash.Test.Support.PolicySimple.Registry do
  @moduledoc false
  use Ash.Registry

  alias Ash.Test.Support.PolicySimple, as: Simple

  entries do
    entry(Simple.User)
    entry(Simple.Organization)
    entry(Simple.Post)
    entry(Simple.Car)
    entry(Simple.CarUser)
    entry(Simple.Trip)
  end
end
