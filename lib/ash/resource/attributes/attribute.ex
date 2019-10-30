defmodule Ash.Resource.Attributes.Attribute do
  defstruct [:name, :type, :ecto_type]

  def new(name, type, _opts \\ []) do
    ecto_type =
      if type == :uuid do
        :binary_id
      else
        type
      end

    %__MODULE__{
      name: name,
      type: type,
      ecto_type: ecto_type
    }
  end
end
