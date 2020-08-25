defmodule Ash.OptionsHelpers do
  @moduledoc false
  def merge_schemas(left, right, section \\ nil) do
    new_right =
      Enum.map(right, fn {key, value} ->
        {key, Keyword.put(value, :subsection, section)}
      end)

    Keyword.merge(left, new_right)
  end

  def map(value) when is_map(value), do: {:ok, value}
  def map(_), do: {:error, "must be a map"}

  def ash_type(type) do
    type = Ash.Type.get_type(type)

    if Ash.Type.ash_type?(type) do
      {:ok, type}
    else
      {:error, "Attribute type must be a built in type or a type module, got: #{inspect(type)}"}
    end
  end

  def default(value) when is_function(value, 0), do: {:ok, value}
  def default({:constant, value}), do: {:ok, {:constant, value}}

  def default({module, function, args})
      when is_atom(module) and is_atom(function) and is_list(args),
      do: {:ok, {module, function, args}}

  def default(nil), do: {:ok, nil}

  def default(other) do
    {:error,
     "#{inspect(other)} is not a valid default. To provide a constant value, use `{:constant, #{
       inspect(other)
     }}`"}
  end

  def make_required!(options, field) do
    Keyword.update!(options, field, &Keyword.put(&1, :required, true))
  end

  def make_optional!(options, field) do
    Keyword.update!(options, field, &Keyword.delete(&1, :required))
  end

  def set_type!(options, field, type) do
    Keyword.update!(options, field, &Keyword.put(&1, :type, type))
  end

  def set_default!(options, field, value) do
    Keyword.update!(options, field, fn config ->
      config
      |> Keyword.put(:default, value)
      |> Keyword.delete(:required)
    end)
  end

  def append_doc!(options, field, to_append) do
    Keyword.update!(options, field, fn opt_config ->
      Keyword.update(opt_config, :doc, to_append, fn existing ->
        existing <> " - " <> to_append
      end)
    end)
  end
end
