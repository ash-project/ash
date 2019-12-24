defmodule Ash.Filter.NotIn do
  defstruct [:values]

  def new(_resource, attr_type, values) do
    casted =
      values
      |> List.wrap()
      |> Enum.reduce({:ok, []}, fn
        value, {:ok, casted} ->
          case Ash.Type.cast_input(attr_type, value) do
            {:ok, value} ->
              {:ok, [value | casted]}

            :error ->
              {:error, "Invalid value #{inspect(value)} for type `not in #{inspect(attr_type)}}`"}
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
