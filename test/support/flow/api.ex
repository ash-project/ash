defmodule Ash.Test.Flow.Api do
  @moduledoc false
  use Ash.Api

  resources do
    resource Ash.Test.Flow.User
    resource Ash.Test.Flow.Org
    allow_unregistered? true
  end
end
