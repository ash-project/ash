defmodule Ash.Resource.Actions.Destroy do
  defstruct [:type, :name, :primary?]

  def new(name, opts \\ []) do
    %__MODULE__{
      name: name,
      type: :destroy,
      primary?: opts[:primary?]
    }
  end
end
