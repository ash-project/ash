defmodule Ash.Resource.Attributes.Attribute do
  defstruct [:name, :type, :ecto_type, :expose?]

  def new(name, type, opts \\ []) do
    ecto_type =
      if type == :uuid do
        :binary_id
      else
        type
      end

    %__MODULE__{
      name: name,
      expose?: opts[:expose?] || false,
      type: type,
      ecto_type: ecto_type
    }
  end
end
