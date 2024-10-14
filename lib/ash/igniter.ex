defmodule Ash.Igniter do
  @moduledoc "Codemods and utilities for working with Ash & Igniter"

  @doc "Adds a codegen task, or updates the name to be `<old_name>_and_name`"
  def codegen(igniter, name) do
    has_codegen? =
      Enum.any?(igniter.tasks, fn
        {"ash.codegen", _args} ->
          true

        _ ->
          false
      end)

    if has_codegen? do
      Map.update!(igniter, :tasks, fn tasks ->
        Enum.map(tasks, fn
          {"ash.codegen", [old_name | rest]} ->
            {"ash.codegen", [old_name <> "_and_#{name}" | rest]}

          task ->
            task
        end)
      end)
    else
      Igniter.add_task(igniter, "ash.codegen", [name])
    end
  end
end
