defmodule Ash.Policy.Test.Simple.Registry do
  @moduledoc false
  use Ash.Registry

  alias Ash.Policy.Test.Simple

  entries do
    entry(Simple.User)
    entry(Simple.Organization)
    entry(Simple.Post)
    entry(Simple.Car)
    entry(Simple.CarUser)
    entry(Simple.Trip)
  end
end
