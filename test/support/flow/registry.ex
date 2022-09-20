defmodule Ash.Test.Support.Flow.Registry do
  @moduledoc false
  use Ash.Registry

  entries do
    entry(Ash.Test.Support.Flow.User)
    entry(Ash.Test.Support.Flow.Org)
  end
end
