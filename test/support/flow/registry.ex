defmodule Ash.Test.Flow.Registry do
  @moduledoc false
  use Ash.Registry

  entries do
    entry(Ash.Test.Flow.User)
    entry(Ash.Test.Flow.Org)
    entry Ash.Test.Flow.ParentResource
    entry Ash.Test.Flow.ChildResource
  end
end
