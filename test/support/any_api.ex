defmodule Ash.Test.AnyApi do
  @moduledoc false
  use Ash.Api

  resources do
    allow_unregistered? true
  end
end
