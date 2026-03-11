# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Reactor.Step do
  @moduledoc """
  Minimal behaviour for reactor step modules invoked by `Ash.Reactor.AshStep`.

  Step modules passed via `ash_step name, impl: MyStep` are called through the
  wrappers in this module so that return values are validated. The callbacks
  mirror the interface expected by `Reactor.Step` (run/3, compensate/4, undo/4).
  """

  @type run_result ::
          {:ok, term()}
          | {:ok, term(), list()}
          | {:ok, term(), list(), list()}
          | {:error, term()}
          | :retry
          | {:halt | :error | :retry, term()}

  @type compensate_result ::
          :ok
          | :retry
          | {:continue, term()}
          | {:error, term()}
          | {:retry, term()}
          | {:ok, term(), list()}
          | {:ok, term(), list(), list()}

  @type undo_result ::
          :ok
          | :retry
          | {:error, term()}
          | {:retry, term()}
          | {:ok, term(), list()}
          | {:ok, term(), list(), list()}

  @callback run(Reactor.inputs(), Reactor.context(), keyword()) :: run_result()
  @callback compensate(any(), Reactor.inputs(), Reactor.context(), keyword()) ::
              compensate_result()
  @callback undo(any(), Reactor.inputs(), Reactor.context(), keyword()) :: undo_result()

  @optional_callbacks compensate: 4, undo: 4

  @doc false
  @spec run(module(), Reactor.inputs(), Reactor.context(), keyword()) :: run_result()
  def run(step_module, arguments, context, options) do
    result = step_module.run(arguments, context, options)

    if valid_run_result?(result) do
      result
    else
      raise Ash.Error.Framework.InvalidReturnType,
        message: """
        Invalid value returned from #{inspect(step_module)}.run/3.

        The callback #{inspect(__MODULE__)}.run/3 expects {:ok, term}, {:ok, term, notifications}, {:ok, term, notifications, new_steps}, {:error, term}, :retry, or {:halt | :error | :retry, term}.
        """
    end
  end

  @doc false
  @spec compensate(module(), any(), Reactor.inputs(), Reactor.context(), keyword()) ::
          compensate_result()
  def compensate(step_module, reason, arguments, context, options) do
    if function_exported?(step_module, :compensate, 4) do
      result = step_module.compensate(reason, arguments, context, options)

      if valid_compensate_result?(result) do
        result
      else
        raise Ash.Error.Framework.InvalidReturnType,
          message: """
          Invalid value returned from #{inspect(step_module)}.compensate/4.

          The callback #{inspect(__MODULE__)}.compensate/4 expects :ok, :retry, {:continue, term}, {:error, term}, {:retry, term}, or {:ok, term, notifications} variants.
          """
      end
    else
      :ok
    end
  end

  @doc false
  @spec undo(module(), any(), Reactor.inputs(), Reactor.context(), keyword()) :: undo_result()
  def undo(step_module, value, arguments, context, options) do
    if function_exported?(step_module, :undo, 4) do
      result = step_module.undo(value, arguments, context, options)

      if valid_undo_result?(result) do
        result
      else
        raise Ash.Error.Framework.InvalidReturnType,
          message: """
          Invalid value returned from #{inspect(step_module)}.undo/4.

          The callback #{inspect(__MODULE__)}.undo/4 expects :ok, :retry, {:error, term}, {:retry, term}, or {:ok, term, notifications} variants.
          """
      end
    else
      :ok
    end
  end

  defp valid_run_result?({:ok, _}), do: true
  defp valid_run_result?({:ok, _, _}), do: true
  defp valid_run_result?({:ok, _, _, _}), do: true
  defp valid_run_result?({:error, _}), do: true
  defp valid_run_result?(:retry), do: true
  defp valid_run_result?({:halt, _}), do: true
  defp valid_run_result?({:retry, _}), do: true
  defp valid_run_result?(_), do: false

  defp valid_compensate_result?(:ok), do: true
  defp valid_compensate_result?(:retry), do: true
  defp valid_compensate_result?({:continue, _}), do: true
  defp valid_compensate_result?({:error, _}), do: true
  defp valid_compensate_result?({:retry, _}), do: true
  defp valid_compensate_result?({:ok, _, _}), do: true
  defp valid_compensate_result?({:ok, _, _, _}), do: true
  defp valid_compensate_result?(_), do: false

  defp valid_undo_result?(:ok), do: true
  defp valid_undo_result?(:retry), do: true
  defp valid_undo_result?({:error, _}), do: true
  defp valid_undo_result?({:retry, _}), do: true
  defp valid_undo_result?({:ok, _, _}), do: true
  defp valid_undo_result?({:ok, _, _, _}), do: true
  defp valid_undo_result?(_), do: false
end
