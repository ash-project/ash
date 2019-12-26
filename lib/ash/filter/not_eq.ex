defmodule Ash.Filter.NotEq do
  defstruct [:value]

  def new(_resource, attr_type, value) do
    case Ash.Type.cast_input(attr_type, value) do
      {:ok, value} ->
        {:ok, %__MODULE__{value: value}}

      :error ->
        {:error, "invalid value #{inspect(value)} for type `!=  #{inspect(attr_type)}`"}
    end
  end

  def strict_subset_of?(
        %{type: :boolean, allow_nil?: false},
        %__MODULE__{value: true},
        %Ash.Filter.NotEq{value: false}
      ) do
    true
  end

  def strict_subset_of?(
        %{type: :boolean, allow_nil?: false},
        %__MODULE__{value: false},
        %Ash.Filter.NotEq{value: true}
      ) do
    true
  end

  def strict_subset_of?(_attr, %__MODULE__{value: value}, %__MODULE__{value: candidate}) do
    value == candidate
  end

  def strict_subset_of?(_attr, %__MODULE__{value: value}, %Ash.Filter.Eq{value: other_value}) do
    value != other_value
  end

  def strict_subset_of?(_attr, %__MODULE__{value: value}, %Ash.Filter.In{values: candidate_values}) do
    value not in candidate_values
  end

  def strict_subset_of?(_, _, _), do: false
end
