defmodule Ash.Resource.Actions.Action do
  defstruct [:type, :name, :path]

  def new(name, type, _opts \\ []) do
    %__MODULE__{
      name: name,
      type: type
    }
  end
end
