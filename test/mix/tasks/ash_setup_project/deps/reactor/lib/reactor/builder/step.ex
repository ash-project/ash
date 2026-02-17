# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Builder.Step do
  @moduledoc """
  Handle building and adding steps to Reactors for the builder.

  You should not use this module directly, but instead use
  `Reactor.Builder.new_step/4` and `Reactor.Builder.add_step/5`.
  """
  alias Reactor.{Argument, Builder, Guard, Step, Template}
  import Reactor.Argument, only: :macros
  import Reactor.Utils

  @option_schema [
    async?: [
      type: {:or, [:boolean, {:mfa_or_fun, 1}]},
      default: true,
      required: false,
      doc: "Allow the step to be run asynchronously?"
    ],
    max_retries: [
      type: {:or, [:non_neg_integer, {:literal, :infinity}]},
      default: 100,
      required: false,
      doc: "The maximum number of times the step can ask to be retried"
    ],
    transform: [
      type:
        {:or,
         [
           nil,
           {:mfa_or_fun, 1},
           {:tuple, [:module, :non_empty_keyword_list]}
         ]},
      required: false,
      doc: "A function which can modify all incoming arguments"
    ],
    context: [
      type: :map,
      required: false,
      default: %{},
      doc: "Context which will be merged with the reactor context when calling this step"
    ],
    description: [
      type: {:or, [nil, :string]},
      required: false,
      doc: "An optional description for the step"
    ],
    guards: [
      type: {:list, {:protocol, Reactor.Guard.Build}},
      required: false,
      default: [],
      doc: "Optional guards to add to the step"
    ],
    ref: [
      type: {:in, [:step_name, :make_ref]},
      required: false,
      default: :make_ref,
      doc: "What sort of step reference to generate"
    ]
  ]

  @doc """
  Build and add a new step to a Reactor.

  ## Arguments

  * `reactor` - An existing Reactor struct to add the step to.
  * `name` - The proposed name of the new step.
  * `impl` - A module implementing the `Reactor.Step` behaviour (or a tuple
    containing the module and options).
  * `arguments` - A list of `Reactor.Argument` structs or shorthand keyword
    lists.

  ## Options

  #{Spark.Options.docs(@option_schema)}
  """
  @doc spark_opts: [{5, @option_schema}]
  @spec add_step(
          Reactor.t(),
          any,
          Builder.impl(),
          [Builder.step_argument()],
          Builder.step_options()
        ) :: {:ok, Reactor.t()} | {:error, any}
  def add_step(reactor, name, impl, arguments, options) do
    with {:ok, options} <- Spark.Options.validate(options, @option_schema),
         {:ok, arguments} <- build_arguments(arguments),
         {:ok, arguments} <- maybe_rewrite_input_arguments(reactor, arguments),
         {:ok, guards} <- build_guards(options[:guards]),
         :ok <- assert_is_step_impl(impl),
         {:ok, {arguments, argument_transform_steps}} <-
           build_argument_transform_steps(arguments, name),
         {:ok, arguments, step_transform_step} <-
           maybe_build_step_transform_all_step(arguments, name, options[:transform]) do
      context =
        if step_transform_step do
          deep_merge(options[:context], %{private: %{replace_arguments: :value}})
        else
          options[:context]
        end

      steps =
        [
          %Step{
            arguments: arguments,
            async?: options[:async?],
            context: context,
            description: options[:description],
            guards: guards,
            impl: impl,
            name: name,
            max_retries: options[:max_retries]
          }
        ]
        |> Enum.concat(argument_transform_steps)
        |> maybe_append(step_transform_step)
        |> Enum.concat(reactor.steps)
        |> Enum.map(&set_ref(&1, options[:ref]))

      {:ok, %{reactor | steps: steps}}
    end
  end

  defp set_ref(step, :step_name), do: %{step | ref: step.name}
  defp set_ref(step, _), do: %{step | ref: make_ref()}

  @doc """
  Dynamically build a new step for later use.

  You're most likely to use this when dynamically returning new steps from an
  existing step.


  ## Arguments

  * `name` - The name of the new step.
  * `impl` - A module implementing the `Reactor.Step` behaviour (or a tuple
    containing the module and options).
  * `arguments` - A list of `Reactor.Argument` structs or shorthand keyword
    lists.

  ## Options

  #{Spark.Options.docs(@option_schema)}
  """
  @doc spark_opts: [{5, @option_schema}]
  @spec new_step(
          any,
          Builder.impl(),
          [Builder.step_argument()],
          Builder.step_options()
        ) ::
          {:ok, Step.t()} | {:error, any}
  def new_step(name, impl, arguments, options) do
    with {:ok, options} <- Spark.Options.validate(options, @option_schema),
         {:ok, arguments} <- build_arguments(arguments),
         :ok <- assert_no_argument_transforms(arguments),
         {:ok, guards} <- build_guards(options[:guards]),
         :ok <- assert_is_step_impl(impl),
         :ok <- validate_no_transform_option(options) do
      step = %Step{
        arguments: arguments,
        async?: options[:async?],
        context: options[:context],
        description: options[:description],
        guards: guards,
        impl: impl,
        name: name,
        max_retries: options[:max_retries]
      }

      {:ok, step}
    end
  end

  defp validate_no_transform_option(options) do
    if Keyword.has_key?(options, :transform) do
      {:error,
       argument_error(:options, "Adding transforms to dynamic steps is not supported.", options)}
    else
      :ok
    end
  end

  defp build_arguments(arguments) do
    arguments
    |> map_while_ok(&Argument.Build.build/1)
    |> and_then(&{:ok, List.flatten(&1)})
  end

  defp maybe_rewrite_input_arguments(reactor, arguments) do
    existing_step_names = MapSet.new(reactor.steps, & &1.name)

    map_while_ok(arguments, fn
      argument when is_from_input(argument) ->
        potential_rewrite_step_name = {:__reactor__, :transform, :input, argument.source.name}

        if MapSet.member?(existing_step_names, potential_rewrite_step_name) do
          {:ok, Argument.from_result(argument.name, potential_rewrite_step_name)}
        else
          {:ok, argument}
        end

      argument
      when is_from_result(argument) or is_from_value(argument) or is_from_element(argument) ->
        {:ok, argument}
    end)
  end

  defp build_guards(guards) do
    guards
    |> map_while_ok(&Guard.Build.build/1)
    |> and_then(&{:ok, List.flatten(&1)})
  end

  defp assert_is_step_impl({impl, opts}) when is_list(opts), do: assert_is_step_impl(impl)

  defp assert_is_step_impl(impl) when is_atom(impl) do
    if Spark.implements_behaviour?(impl, Step) do
      :ok
    else
      {:error,
       argument_error(:impl, "Module does not implement the `Reactor.Step` behaviour.", impl)}
    end
  end

  defp build_argument_transform_steps(arguments, step_name) do
    arguments
    |> reduce_while_ok({[], []}, fn
      argument, {arguments, steps} when is_from_input(argument) and has_transform(argument) ->
        transform_step_name = {:__reactor__, :transform, argument.name, step_name}

        step =
          build_transform_step(
            argument.source,
            transform_step_name,
            argument.transform
          )

        argument = %Argument{
          name: argument.name,
          source: %Template.Result{name: transform_step_name}
        }

        {:ok, {[argument | arguments], [%{step | transform: nil} | steps]}}

      argument, {arguments, steps} when is_from_result(argument) and has_transform(argument) ->
        transform_step_name = {:__reactor__, :transform, argument.name, step_name}

        step =
          build_transform_step(
            argument.source,
            transform_step_name,
            argument.transform
          )

        argument = %Argument{
          name: argument.name,
          source: %Template.Result{name: transform_step_name}
        }

        {:ok, {[argument | arguments], [%{step | transform: nil} | steps]}}

      argument, {arguments, steps} ->
        {:ok, {[argument | arguments], steps}}
    end)
  end

  defp maybe_build_step_transform_all_step(arguments, _name, nil), do: {:ok, arguments, nil}

  defp maybe_build_step_transform_all_step(arguments, name, transform)
       when is_function(transform, 1),
       do:
         maybe_build_step_transform_all_step(arguments, name, {Step.TransformAll, fun: transform})

  defp maybe_build_step_transform_all_step(arguments, name, transform) do
    step = %Step{
      arguments: arguments,
      async?: false,
      impl: transform,
      name: {:__reactor__, :transform, name},
      max_retries: 0
    }

    {:ok, [Argument.from_result(:value, step.name)], step}
  end

  defp build_transform_step(argument_source, step_name, transform) when is_function(transform, 1),
    do: build_transform_step(argument_source, step_name, {Step.Transform, fun: transform})

  defp build_transform_step(argument_source, step_name, transform)
       when tuple_size(transform) == 2 and is_atom(elem(transform, 0)) and
              is_list(elem(transform, 1)) do
    %Step{
      arguments: [
        %Argument{
          name: :value,
          source: argument_source
        }
      ],
      async?: false,
      impl: transform,
      name: step_name,
      max_retries: 0
    }
  end

  defp assert_no_argument_transforms(arguments) do
    arguments
    |> Enum.reject(&is_nil(&1.transform))
    |> case do
      [] ->
        :ok

      [argument] ->
        {:error,
         argument_error(
           :arguments,
           "Argument `#{argument.name}` has a transform attached.",
           argument
         )}

      arguments ->
        sentence = sentence(arguments, &"`#{&1.name}`", ", ", " and ")

        {:error,
         argument_error(:arguments, "Arguments #{sentence} have transforms attached.", arguments)}
    end
  end
end
