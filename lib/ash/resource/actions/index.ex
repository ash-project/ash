defmodule Ash.Resource.Actions.Index do
  defstruct [:type, :name, :primary?]

  def new(name, opts \\ []) do
    %__MODULE__{
      name: name,
      type: :index,
      primary?: opts[:primary?]
    }
  end
end
