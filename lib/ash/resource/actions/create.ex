defmodule Ash.Resource.Actions.Create do
  defstruct [:type, :name, :primary?]

  def new(name, opts \\ []) do
    %__MODULE__{
      name: name,
      type: :create,
      primary?: opts[:primary?]
    }
  end
end
