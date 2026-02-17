# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Executor.Init do
  @moduledoc """
  Handle argument checking and state setup for a Reactor run.
  """

  alias Reactor.Executor
  import Reactor, only: :macros
  import Reactor.Utils

  @doc false
  @spec init(
          Reactor.t(),
          Reactor.inputs(),
          Reactor.context(),
          Reactor.run_options() | Reactor.undo_options()
        ) ::
          {:ok, Reactor.t(), state :: map} | {:error, any}
  def init(reactor, _inputs, _context, _options) when not is_reactor(reactor),
    do: {:error, ArgumentError.exception(message: "`reactor` is not a Reactor.")}

  def init(reactor, inputs, context, options) do
    with {:ok, inputs} <- into_map(inputs),
         {:ok, inputs} <- validate_inputs(reactor, inputs),
         {:ok, context} <- into_map(context),
         {:ok, options} <- into_map(options) do
      state = Executor.State.init(options)

      context =
        reactor.context
        |> deep_merge(context)
        |> deep_merge(%{private: %{inputs: inputs}})
        |> Map.put_new(:run_id, state.run_id)

      {:ok, %{reactor | context: context}, state}
    end
  end

  defp into_map(map) when is_map(map), do: {:ok, map}

  defp into_map(mappish) do
    {:ok, Map.new(mappish)}
  rescue
    _error in [Protocol.UndefinedError, ArgumentError] ->
      {:error,
       ArgumentError.exception(message: "`#{inspect(mappish)}` cannot be converted into a map.")}
  end

  defp validate_inputs(reactor, inputs) do
    valid_input_names = MapSet.new(reactor.inputs, & &1.name)
    provided_input_names = inputs |> Map.keys() |> MapSet.new()

    if MapSet.subset?(valid_input_names, provided_input_names) do
      {:ok, Map.take(inputs, Enum.to_list(valid_input_names))}
    else
      missing_inputs =
        valid_input_names
        |> MapSet.difference(provided_input_names)
        |> sentence(&"`#{inspect(&1)}`", ", ", " and ")

      {:error,
       ArgumentError.exception(
         message: "Reactor is missing the following inputs; #{missing_inputs}"
       )}
    end
  end
end
