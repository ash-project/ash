defmodule Ash.Authorization.Request do
  defstruct [:resource, :rules, :filter]

  def new(opts) do
    struct!(__MODULE__, opts)
  end
end
