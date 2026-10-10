# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Preparation.EmitSignal do
  @moduledoc false
  # See `Ash.Resource.Preparation.Builtins.emit_signal/3`.
  use Ash.Resource.Preparation

  alias Ash.Signals.Emitter

  @impl true
  def init(opts), do: Emitter.init(opts)

  @impl true
  def supports(_opts), do: [Ash.ActionInput]

  @impl true
  def prepare(input, opts, _context) do
    case opts[:phase] do
      :before_action ->
        Ash.ActionInput.before_action(input, fn input ->
          case emit(input, opts, nil) do
            :ok -> input
            {:error, error} -> Ash.ActionInput.add_error(input, error)
          end
        end)

      :after_action ->
        Ash.ActionInput.after_action(input, fn input, result ->
          with :ok <- emit(input, opts, result) do
            if input.action.returns, do: {:ok, result}, else: :ok
          end
        end)

      :after_transaction ->
        Ash.ActionInput.after_transaction(input, fn
          input, :ok ->
            emit(input, opts, nil)

          input, {:ok, result} ->
            with :ok <- emit(input, opts, result) do
              {:ok, result}
            end

          _input, result ->
            result
        end)
    end
  end

  # unmapped signal fields come from arguments of the same name, and `fields` from the result
  defp emit(input, opts, result) do
    payload =
      Emitter.payload(
        opts,
        &Map.fetch(input.arguments, &1),
        fn field -> if is_map(result), do: Map.fetch(result, field), else: :error end,
        %{}
      )

    Ash.Signals.emit(input, opts[:signal_module], opts[:signal], payload)
  end
end
