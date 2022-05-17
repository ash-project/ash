defmodule Ash.Policy.Test.Simple.Api do
  @moduledoc false
  use Ash.Api

  resources do
    registry(Ash.Policy.Test.Simple.Registry)
  end
end
