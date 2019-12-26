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

  def contains?(%__MODULE__{values: values}, %__MODULE__{values: candidate_values}) do
    Enum.all?(candidate_values, fn candidate -> candidate not in values end)
  end

  def contains?(%__MODULE__{values: values}, %Ash.Filter.Eq{value: candidate}) do
    candidate not in values
  end

  # TODO: Something like `foo != 1 and foo != 2` should be considered as contained in the
  # filter `not in [1, 2]`. This will only ever work if we simplify "and" filters wherever
  # possible.
  def contains?(%__MODULE__{values: [single_value]}, %Ash.Filter.NotEq{value: candidate}) do
    single_value == candidate
  end

  def contains?(_, _), do: false
end
