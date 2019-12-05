defmodule Ash.Resource.Actions.Read do
  defstruct [:type, :name, :primary?, :paginate?, :rules]

  def new(name, opts \\ []) do
    %__MODULE__{
      name: name,
      type: :read,
      primary?: opts[:primary?],
      paginate?: opts[:paginate?],
      rules: opts[:rules]
    }
  end
end
