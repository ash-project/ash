defmodule Ash.Filter.Eq do
  defstruct [:value]

  def new(_resource, attr_name, attr_type, value) do
    case Ash.Type.cast_input(attr_type, value) do
      {:ok, value} ->
        {:ok, %__MODULE__{value: value}}

      :error ->
        {:error,
         Ash.Error.Filter.InvalidFilterValue.exception(
           filter: %__MODULE__{value: value},
           value: value,
           field: attr_name
         )}
    end
  end
end
