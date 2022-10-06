defmodule Ash.Test.Flow.Api do
  @moduledoc false
  use Ash.Api

  resources do
    registry Ash.Test.Flow.Registry
  end
end
