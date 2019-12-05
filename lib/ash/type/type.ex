defmodule Ash.Type do
  def builtins(), do: [:string, :uuid, :utc_datetime]

  def ash_type?(module) do
    :erlang.function_exported(module, :module_info, 0) and ash_type_module?(module)
  end

  defp ash_type_module?(module) do
    :attributes
    |> module.module_info()
    |> Keyword.get(:behaviour, [])
    |> Enum.any?(&(&1 == __MODULE__))
  end
end
