defmodule Ash.Resource.Actions.Update do
  defstruct [:type, :name, :primary?]

  def new(name, opts \\ []) do
    %__MODULE__{
      name: name,
      type: :update,
      primary?: opts[:primary?]
    }
  end
end
