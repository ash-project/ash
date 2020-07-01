defmodule Ash.Actions.PrimaryKeyHelpers do
  @moduledoc false
  def values_to_primary_key_filters(destination, identifiers) do
    Enum.reduce(identifiers, {:ok, []}, fn
      identifier, {:ok, filters} ->
        case value_to_primary_key_filter(destination, identifier) do
          {:ok, filter} ->
            {:ok, [filter | filters]}

          {:error, error} ->
            {:error, error}
        end

      _, {:error, error} ->
        {:error, error}
    end)
  end

  def value_to_primary_key_filter(resource, value) do
    do_value_to_primary_key_filter(resource, Ash.primary_key(resource), value)
  end

  defp do_value_to_primary_key_filter(_resource, [], _value), do: {:error, :no_primary_key}

  defp do_value_to_primary_key_filter(resource, primary_key, value) when is_list(value) do
    if Keyword.keyword?(value) do
      do_value_to_primary_key_filter(resource, primary_key, Enum.into(value, %{}))
    else
      {:error, "Invalid primary key #{inspect(value)}"}
    end
  end

  defp do_value_to_primary_key_filter(resource, primary_key, value) when is_map(value) do
    if Enum.all?(primary_key, &Map.has_key?(value, &1)) do
      value
      |> Map.take(primary_key)
      |> Enum.reduce({:ok, []}, fn
        {key, val}, {:ok, filter} ->
          attr = Ash.attribute(resource, key)

          case Ash.Type.cast_input(attr.type, val) do
            {:ok, casted} ->
              {:ok, Keyword.put(filter, attr.name, casted)}

            _ ->
              {:error, "#{key} is invalid"}
          end

        _, {:error, error} ->
          {:error, error}
      end)
    else
      {:error, "Invalid primary key #{inspect(value)}"}
    end
  end

  defp do_value_to_primary_key_filter(resource, [field], value) do
    do_value_to_primary_key_filter(resource, [field], [{field, value}])
  end

  defp do_value_to_primary_key_filter(_, _, _), do: {:error, ["Invalid primary key"]}
end
