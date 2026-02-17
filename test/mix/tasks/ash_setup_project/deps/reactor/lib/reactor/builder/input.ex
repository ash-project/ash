# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Builder.Input do
  @moduledoc """
  Handle adding inputs to Reactors for the builder.

  You should not use this module directly, but instead use
  `Reactor.Builder.add_input/3`.
  """
  alias Reactor.{Argument, Step}

  @options Spark.Options.new!(
             transform: [
               type:
                 {:or,
                  [
                    nil,
                    {:mfa_or_fun, 1},
                    {:tuple, [:module, :non_empty_keyword_list]}
                  ]},
               required: false,
               default: nil,
               doc: """
               An optional transformation function which can be used to modify the input before it is used in the Reactor.
               """
             ],
             description: [
               type: {:or, [nil, :string]},
               required: false,
               default: nil,
               doc: """
               An optional description for the input.
               """
             ]
           )

  @type transform :: nil | (any -> any) | {Step.step(), keyword}
  @type options :: [{:description, nil | String.t()}, {:transform, transform}] | transform

  @doc """
  Add a named input to the reactor.
  """
  @spec add_input(Reactor.t(), any, options | transform) :: {:ok, Reactor.t()} | {:error, any}
  def add_input(reactor, name, nil), do: add_input(reactor, name, [])

  def add_input(reactor, name, options) when is_list(options) do
    with {:ok, options} <- Spark.Options.validate(options, @options) do
      reactor =
        reactor
        |> do_add_input(name, options)
        |> maybe_add_input_transform(name, options[:transform])

      {:ok, reactor}
    end
  end

  def add_input(reactor, name, transform), do: add_input(reactor, name, transform: transform)

  defp do_add_input(reactor, name, options) do
    input = %Reactor.Input{
      name: name,
      description: options[:description]
    }

    %{reactor | inputs: [input | reactor.inputs]}
  end

  defp maybe_add_input_transform(reactor, _name, nil), do: reactor

  defp maybe_add_input_transform(reactor, name, {module, options} = transform)
       when is_atom(module) and is_list(options) do
    transform_step = %Step{
      arguments: [Argument.from_input(:value, name)],
      async?: true,
      description: "Transformed result of the `#{inspect(name)}` step",
      impl: transform,
      name: {:__reactor__, :transform, :input, name},
      max_retries: 0,
      ref: make_ref()
    }

    %{reactor | steps: [transform_step | reactor.steps]}
  end

  defp maybe_add_input_transform(reactor, name, transform),
    do: maybe_add_input_transform(reactor, name, {Step.Transform, fun: transform})
end
