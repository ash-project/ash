defmodule Ash.Authorization.Request do
  defstruct [:resource, :authorization_steps, :filter, :strict_access?]

  @type t :: %__MODULE__{
          resource: Ash.resource(),
          authorization_steps: list(term),
          filter: Ash.Filter.t(),
          strict_access?: boolean
        }

  def new(opts) do
    struct!(__MODULE__, opts)
  end
end
