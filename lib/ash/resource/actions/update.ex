defmodule Ash.Resource.Actions.Update do
  defstruct [:type, :name, :primary?, :rules]

  def new(name, opts \\ []) do
    %__MODULE__{
      name: name,
      type: :update,
      primary?: opts[:primary?],
      rules: opts[:rules]
    }
  end
end
