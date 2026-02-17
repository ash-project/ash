# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Dsl.Verifier do
  @moduledoc """
  Runs `Reactor.Dsl.Build.verify/2` for all the entities in the reactor.
  """
  use Spark.Dsl.Verifier
  alias Reactor.{Argument, Dsl.Build, Dsl.Input}
  alias Spark.{Dsl.Verifier, Error.DslError}
  import Reactor.Dsl.Utils
  require Argument

  @doc false
  @impl true
  @spec verify(Spark.Dsl.t()) :: :ok | {:error, any}
  def verify(dsl_state) do
    with :ok <- recursively_verify_step_names(dsl_state) do
      recursively_verify_steps(dsl_state)
    end
  end

  defp recursively_verify_step_names(dsl_state) do
    dsl_state
    |> Verifier.get_entities([:reactor])
    |> recursively_extract_step_names()
    |> assert_unique_step_names(dsl_state)
  end

  defp recursively_extract_step_names(steps), do: recursively_extract_step_names(steps, [])
  defp recursively_extract_step_names([], names), do: names

  defp recursively_extract_step_names([step | steps], names) when is_list(step.steps) do
    names = recursively_extract_step_names(step.steps, [step.name | names])
    recursively_extract_step_names(steps, names)
  end

  defp recursively_extract_step_names([step | steps], names),
    do: recursively_extract_step_names(steps, [step.name | names])

  defp recursively_verify_steps(dsl_state) do
    dsl_state
    |> Verifier.get_entities([:reactor])
    |> Enum.reject(&is_struct(&1, Input))
    |> Enum.reduce_while(:ok, fn step, :ok ->
      case recursively_verify_step(step, nil, dsl_state) do
        :ok -> {:cont, :ok}
        {:error, reason} -> {:halt, {:error, reason}}
      end
    end)
  end

  defp recursively_verify_step(%{step: [_ | _]} = step, parent_step, dsl_state) do
    with :ok <- verify_step(step, parent_step, dsl_state) do
      Enum.reduce_while(step.steps, :ok, fn child, :ok ->
        case recursively_verify_step(child, step, dsl_state) do
          :ok -> {:cont, :ok}
          {:error, reason} -> {:halt, {:error, reason}}
        end
      end)
    end
  end

  defp recursively_verify_step(step, parent_step, dsl_state),
    do: verify_step(step, parent_step, dsl_state)

  defp verify_step(step, parent_step, dsl_state) do
    with :ok <- maybe_verify_element_arguments(step, parent_step, dsl_state) do
      Build.verify(step, dsl_state)
    end
  end

  defp maybe_verify_element_arguments(step, parent_step, dsl_state)
       when parent_step.iterable? == true do
    step.arguments
    |> Enum.reduce_while(:ok, fn
      argument, :ok
      when Argument.is_from_element(argument) and argument.source.name == parent_step.name ->
        {:cont, :ok}

      argument, :ok when Argument.is_from_element(argument) ->
        {:halt,
         {:error,
          DslError.exception(
            module: Verifier.get_persisted(dsl_state, :module),
            path: [:reactor, step.name, :argument, argument.name],
            message: """
            Element template refers to non-parent step.

            The argument `#{inspect(argument.name)}` is sourced from an element template,
            however this template refers to a step which is not it's immediate parent.  This
            is an unsupported configuration.
            """
          )}}

      _argument, :ok ->
        {:cont, :ok}
    end)
  end

  defp maybe_verify_element_arguments(step, _parent_step, dsl_state)
       when is_map_key(step, :arguments) do
    step.arguments
    |> Enum.reduce_while(:ok, fn
      argument, :ok when Argument.is_from_element(argument) ->
        {:halt,
         {:error,
          DslError.exception(
            module: Verifier.get_persisted(dsl_state, :module),
            path: [:reactor, step.name, :argument, argument.name],
            message: """
            Unsupported element template in argument.

            The argument `#{inspect(argument.name)}` is sourced from an element template,
            this is fine if it's being passed to a step which is an iterator however the
            step type `#{inspect(step.__struct__)}` isn't an iterator.

            If you're defining your own iterable step type then you need to add the
            `iterable?` field to its struct with its value set to `true`.
            """
          )}}

      _argument, :ok ->
        {:cont, :ok}
    end)
  end

  defp maybe_verify_element_arguments(_step, _parent_step, _dsl_state), do: :ok
end
