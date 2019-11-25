defmodule Ash.Resource.Actions.Destroy do
  defstruct [:type, :name, :primary?, :rules]

  def new(name, opts \\ []) do
    %__MODULE__{
      name: name,
      type: :destroy,
      primary?: opts[:primary?],
      rules: opts[:rules]
    }
  end
end
