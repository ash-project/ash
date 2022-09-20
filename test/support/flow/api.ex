defmodule Ash.Test.Support.Flow.Api do
  @moduledoc false
  use Ash.Api

  resources do
    registry Ash.Test.Support.Flow.Registry
  end
end
