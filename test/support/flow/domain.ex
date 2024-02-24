defmodule Ash.Test.Flow.Domain do
  @moduledoc false
  use Ash.Domain

  resources do
    resource Ash.Test.Flow.User
    resource Ash.Test.Flow.Org
    allow_unregistered? true
  end
end
