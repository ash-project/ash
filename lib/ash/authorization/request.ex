defmodule Ash.Authorization.Request do
  defstruct [:resource, :authorization_steps, :filter]

  @type t :: %__MODULE__{
          resource: Ash.resource(),
          authorization_steps: list(term),
          filter: Ash.Filter.t()
        }

  def new(opts) do
    struct!(__MODULE__, opts)
  end
end
