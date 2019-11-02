defmodule Ash.Resource.Actions.Delete do
  defstruct [:type, :name, :primary?]

  def new(name, opts \\ []) do
    %__MODULE__{
      name: name,
      type: :delete,
      primary?: opts[:primary?]
    }
  end
end
