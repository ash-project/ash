defmodule Ash.Resource.Actions.Create do
  defstruct [:type, :name, :primary?, :rules]

  def new(name, opts \\ []) do
    %__MODULE__{
      name: name,
      type: :create,
      primary?: opts[:primary?],
      rules: opts[:rules]
    }
  end
end
