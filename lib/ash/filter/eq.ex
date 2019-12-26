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

  def strict_subset_of?(_attribute, %__MODULE__{value: value}, %__MODULE__{value: candidate}) do
    value == candidate
  end

  def strict_subset_of?(_attribute, %__MODULE__{value: value}, %Ash.Filter.In{values: [candidate]}) do
    value == candidate
  end

  def strict_subset_of?(_, _, _), do: false
end
