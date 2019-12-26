defmodule Ash.Filter.In do
  defstruct [:values]

  def new(resource, attr_type, [value]) do
    Ash.Filter.Eq.new(resource, attr_type, value)
  end

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
              {:error, "Invalid value #{inspect(value)} for type `in #{inspect(attr_type)}`"}
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

  def strict_subset_of?(_attr, %__MODULE__{values: values}, %__MODULE__{values: candidate_values}) do
    Enum.all?(candidate_values, fn candidate -> candidate in values end)
  end

  def strict_subset_of?(_attr, %__MODULE__{values: values}, %Ash.Filter.Eq{value: candidate}) do
    candidate in values
  end

  def strict_subset_of?(_attr, _, _), do: false
end
