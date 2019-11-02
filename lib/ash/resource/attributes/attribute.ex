defmodule Ash.Resource.Attributes.Attribute do
  defstruct [:name, :type, :ecto_type, :primary_key?]

  def new(name, type, opts \\ []) do
    # TODO: Remove `ecto_type` here and do that mapping in
    # the database layer
    ecto_type =
      if type == :uuid do
        :binary_id
      else
        type
      end

    %__MODULE__{
      name: name,
      type: type,
      ecto_type: ecto_type,
      primary_key?: opts[:primary_key?] || false
    }
  end
end
