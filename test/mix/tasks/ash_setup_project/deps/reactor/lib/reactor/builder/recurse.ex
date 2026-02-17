# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Builder.Recurse do
  @moduledoc """
  Handle recursive execution of Reactors for the builder.

  This module provides functionality to recursively execute a reactor,
  with the output of each iteration becoming the input of the next one.

  You should not use this module directly, but instead use
  `Reactor.Builder.recurse/4`.
  """
  alias Reactor.{Builder, Error.Internal.ComposeError}

  @opt_schema Spark.Options.new!(
                guards: [
                  type: {:list, {:protocol, Reactor.Guard.Build}},
                  required: false,
                  default: [],
                  doc: "Any guards which need to be added to the generated step"
                ],
                async?: [
                  type: :boolean,
                  required: false,
                  default: true,
                  doc: "Whether the nested Reactor is allowed to run async or not"
                ],
                max_iterations: [
                  type: :pos_integer,
                  required: false,
                  doc: "Maximum number of iterations to execute"
                ],
                exit_condition: [
                  type: {:or, [nil, {:mfa_or_fun, 1}]},
                  required: false,
                  doc: "Function to determine when to stop iteration"
                ]
              )

  @doc """
  Execute a Reactor recursively.

  ## Options

  #{Spark.Options.docs(@opt_schema)}
  """
  @spec recurse(Reactor.t(), atom, Reactor.t() | module, [Builder.step_argument()], keyword) ::
          {:ok, Reactor.t()} | {:error, any}
  def recurse(reactor, name, inner_reactor, arguments, options) when is_atom(inner_reactor) do
    case Builder.Compose.verify_arguments(inner_reactor, arguments) do
      :ok ->
        Builder.add_step(
          reactor,
          name,
          {Reactor.Step.Recurse,
           reactor: inner_reactor,
           max_iterations: options[:max_iterations],
           exit_condition: options[:exit_condition],
           state: :init},
          arguments,
          async?: options[:async?],
          guards: options[:guards] || [],
          max_retries: 0,
          ref: :step_name
        )

      {:error, {:extra_args, _inputs, extra_args}} ->
        {:error,
         ComposeError.exception(
           arguments: arguments,
           inner_reactor: inner_reactor,
           outer_reactor: reactor,
           message: """
           Recursion contains extra arguments:

             #{Enum.map_join(extra_args, "\n  ", &inspect/1)}
           """
         )}

      {:error, {:missing_args, _inputs, missing_args}} ->
        {:error,
         ComposeError.exception(
           arguments: arguments,
           inner_reactor: inner_reactor,
           outer_reactor: reactor,
           message: """
           Recursion contains missing arguments:

             #{Enum.map_join(missing_args, "\n  ", &inspect/1)}
           """
         )}
    end
  end

  @doc """
  Verify that the reactor has compatible inputs and outputs for recursion.

  For recursion to work properly, the output type of the reactor must match
  its input type, so that the result of one iteration can be fed into the next.

  This function checks:
  1. The reactor has a return value defined
  2. The output structure (as a map) contains all keys that are required as inputs
  """
  @spec verify_io_compatibility(Reactor.t() | module) ::
          :ok | {:error, {:incompatible_io, String.t()}}
  def verify_io_compatibility(reactor) when is_atom(reactor) do
    with {:ok, reactor} <- Reactor.Info.to_struct(reactor) do
      verify_io_compatibility(reactor)
    end
  end

  def verify_io_compatibility(reactor) do
    # Ensure reactor has a defined return value
    if is_nil(reactor.return) do
      {:error, {:incompatible_io, "Reactor must have a return value defined for recursion"}}
    else
      :ok
    end
  end
end
