defmodule Ash.Resource.Actions.Read do
  defstruct [:type, :name, :primary?]

  def new(name, opts \\ []) do
    %__MODULE__{
      name: name,
      type: :read,
      primary?: opts[:primary?]
    }
  end
end
