defmodule Ash.Filter.Eq do
  defstruct [:value]

  def new(_resource, attr_type, value) do
    case Ash.Type.cast_input(attr_type, value) do
      {:ok, value} ->
        {:ok, %__MODULE__{value: value}}

      :error ->
        {:error, "Invalid value #{inspect(value)} for type `== #{inspect(attr_type)}`"}
    end
  end
end
