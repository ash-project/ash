defmodule Ash.Resource.Actions.Get do
  defstruct [:type, :name, :primary?]

  def new(name, opts \\ []) do
    %__MODULE__{
      name: name,
      type: :get,
      primary?: opts[:primary?]
    }
  end
end
