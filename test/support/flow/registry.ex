defmodule Ash.Test.Flow.Registry do
  @moduledoc false
  use Ash.Registry

  entries do
    entry(Ash.Test.Flow.User)
    entry(Ash.Test.Flow.Org)
  end
end
