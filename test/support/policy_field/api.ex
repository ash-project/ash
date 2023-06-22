defmodule Ash.Test.Support.PolicyField.Api do
  @moduledoc false
  use Ash.Api

  resources do
    allow_unregistered? true
  end
end
