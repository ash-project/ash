# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Builder.Compose do
  @moduledoc """
  Handle composition of Reactors for the builder.

  The composition logic was getting complicated enough that it seemed sensible
  to extract it from the builder - if only to aid readability.

  You should not use this module directly, but instead use
  `Reactor.Builder.compose/4`.
  """
  alias Reactor.{Builder, Error.Internal.ComposeError}

  @opt_schema Spark.Options.new!(
                allow_async?: [
                  type: :boolean,
                  required: false,
                  default: true,
                  doc: "Whether the composed reactor is allowed to run its steps asynchronously"
                ],
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
                support_undo?: [
                  type: :boolean,
                  required: false,
                  default: true,
                  doc: """
                  Whether the composed reactor should also be undone on failure.
                  """
                ]
              )

  @doc """
  Compose another Reactor inside this one.

  ## Options

  #{Spark.Options.docs(@opt_schema)}
  """
  @spec compose(Reactor.t(), atom, Reactor.t() | module, [Builder.step_argument()], keyword) ::
          {:ok, Reactor.t()} | {:error, any}
  def compose(reactor, name, inner_reactor, arguments, options) when is_atom(inner_reactor) do
    case verify_arguments(inner_reactor, arguments) do
      :ok ->
        Builder.add_step(
          reactor,
          name,
          {Reactor.Step.Compose,
           reactor: inner_reactor,
           allow_async?: options[:allow_async?],
           support_undo?: options[:support_undo?]},
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
           Composition contains extra arguments:

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
           Composition contains missing arguments:

             #{Enum.map_join(missing_args, "\n  ", &inspect/1)}
           """
         )}
    end
  end

  @doc "Verify that the arguments and reactor inputs match"
  @spec verify_arguments(Reactor.t() | module, [Builder.step_argument()]) ::
          :ok | {:error, {:extra_args | :missing_args, MapSet.t(), MapSet.t()}}
  def verify_arguments(reactor, arguments) when is_atom(reactor) do
    with {:ok, reactor} <- Reactor.Info.to_struct(reactor) do
      verify_arguments(reactor, arguments)
    end
  end

  def verify_arguments(reactor, arguments) do
    with {:ok, inputs} <- reactor_inputs(reactor),
         {:ok, arg_names} <- argument_names(arguments) do
      extra_args =
        arg_names
        |> MapSet.difference(inputs)
        |> Enum.to_list()

      missing_args =
        inputs
        |> MapSet.difference(arg_names)
        |> Enum.to_list()

      case {extra_args, missing_args} do
        {[], []} ->
          :ok

        {extra_args, []} ->
          {:error, {:extra_args, inputs, extra_args}}

        {[], missing_args} ->
          {:error, {:missing_args, inputs, missing_args}}

        {_extra_args, missing_args} ->
          # Both extra and missing args - report missing args as primary issue
          {:error, {:missing_args, inputs, missing_args}}
      end
    end
  end

  defp reactor_inputs(reactor) when is_struct(reactor),
    do: {:ok, MapSet.new(reactor.inputs, & &1.name)}

  defp reactor_inputs(reactor) when is_atom(reactor) do
    with {:ok, reactor} <- Reactor.Info.to_struct(reactor) do
      reactor_inputs(reactor)
    end
  end

  defp argument_names(arguments) do
    argument_names =
      arguments
      |> Enum.reject(&is_struct(&1, Reactor.Dsl.WaitFor))
      |> MapSet.new(& &1.name)

    {:ok, argument_names}
  end
end
