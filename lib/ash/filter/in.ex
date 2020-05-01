defmodule Ash.Filter.In do
  defstruct [:values]

  def new(resource, attr_name, attr_type, [value]) do
    Ash.Filter.Eq.new(resource, attr_name, attr_type, value)
  end

  def new(_resource, attr_name, attr_type, values) do
    casted =
      values
      |> List.wrap()
      |> Enum.reduce({:ok, []}, fn
        value, {:ok, casted} ->
          case Ash.Type.cast_input(attr_type, value) do
            {:ok, value} ->
              {:ok, [value | casted]}

            :error ->
              {:error,
               Ash.Error.Filter.InvalidFilterValue.exception(
                 filter: %__MODULE__{values: values},
                 value: value,
                 field: attr_name
               )}
          end

        _, {:error, error} ->
          {:error, error}
      end)

    case casted do
      {:error, error} ->
        {:error, error}

      {:ok, values} ->
        {:ok, %__MODULE__{values: values}}
    end
  end
end
