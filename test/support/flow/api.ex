defmodule Ash.Test.Flow.Api do
  @moduledoc false
  use Ash.Api

  resources do
    registry Ash.Test.Flow.Registry
    allow_unregistered? true
  end
end
