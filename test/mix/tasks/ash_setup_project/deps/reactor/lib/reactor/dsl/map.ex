# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Dsl.Map do
  @moduledoc """
  The `map` DSL entity struct.

  See `d:Reactor.map`.
  """

  defstruct __identifier__: nil,
            allow_async?: false,
            arguments: [],
            batch_size: 100,
            description: nil,
            guards: [],
            iterable?: true,
            name: nil,
            return: nil,
            source: nil,
            steps: [],
            strict_ordering?: true,
            __spark_metadata__: nil

  alias Reactor.{Builder, Dsl, Step, Template}

  @type t :: %Dsl.Map{
          __identifier__: any,
          allow_async?: boolean,
          arguments: [Dsl.Argument.t()],
          batch_size: pos_integer(),
          description: nil | String.t(),
          guards: [Dsl.Where.t() | Dsl.Guard.t()],
          iterable?: true,
          name: atom,
          return: atom,
          source: Template.t(),
          steps: [Dsl.Step.t()],
          strict_ordering?: boolean,
          __spark_metadata__: Spark.Dsl.Entity.spark_meta()
        }

  @doc false
  def __entity__,
    do: %Spark.Dsl.Entity{
      name: :map,
      describe: """
      Execute nested steps for every item of an iterator.

      Allows you to "map over" a collection applying a some steps to each element,
      returning a list of new values.  The input collection must be bounded - ie you
      cannot pass infinite streams into this step or it will just loop forever - and
      because it has to keep the results from each batch will eventually just use up
      all available RAM.

      Under the hood we use `Iter` to handle lazy iteration of the collection.  This
      means that you can pass an `Iter.t` or any value for which `Iter.IntoIterable`
      is implemented.

      > #### A note on ordering {: .tip}
      >
      > If your application doesn't need the results back in the same order that they
      > were provided then setting `strict_ordering?` to `false` will increase
      > performance - especially on large input sets.
      """,
      examples: [
        """
        map :double_numbers do
          source input(:numbers)

          step :double do
            argument :number, element(:double_numbers)

            run fn %{number: number}, _ ->
              {:ok, number * 2}
            end
          end
        end
        """,
        """
        step :get_subscriptions do
          run fn _, _ ->
            Stripe.Subscription.list()
          end
        end

        map :cancel_subscriptions do
          source result(:get_subscriptions)

          step :cancel do
            argument :sub_id, element(:cancel_subscriptions, [:id])

            run fn args, _ ->
              Stripe.Subscription.cancel(arg.sub_id, %{prorate: true, invoice_now: true})
            end
          end

          return :cancel
        end
        """
      ],
      target: Dsl.Map,
      args: [:name],
      identifier: :name,
      imports: [Dsl.Argument],
      entities: [
        arguments: [Dsl.Argument.__entity__(), Dsl.WaitFor.__entity__()],
        guards: [Dsl.Where.__entity__(), Dsl.Guard.__entity__()],
        steps: []
      ],
      recursive_as: :steps,
      schema: [
        name: [
          type: :atom,
          required: true,
          doc: """
          A unique name for the step.
          """
        ],
        allow_async?: [
          type: :boolean,
          required: false,
          default: false,
          doc: """
          Whether the emitted steps should be allowed to run asynchronously.
          """
        ],
        batch_size: [
          type: :pos_integer,
          required: false,
          default: 100,
          doc: """
          The number of items to consume off the source when emitting steps.
          """
        ],
        description: [
          type: :string,
          required: false,
          doc: """
          An optional description for the step.
          """
        ],
        source: [
          type: Template.type(),
          required: true,
          doc: """
          The iterator or enumerable to use as the source of the iteration.
          """
        ],
        return: [
          type: :atom,
          required: false,
          doc: """
          The name of the nested step to use as the return value.
          """
        ],
        strict_ordering?: [
          type: :boolean,
          required: false,
          default: true,
          doc: """
          Whether the mapped values must be returned in the same order that they were provided.
          """
        ]
      ]
    }

  defimpl Dsl.Build do
    import Reactor.Utils
    alias Reactor.Argument
    alias Spark.{Dsl.Verifier, Error.DslError}

    def build(map, reactor) do
      build(map, reactor, %{map_arguments: []})
    end

    def build(map, reactor, context) do
      sub_reactor = Builder.new(reactor.id)

      # Add current map's arguments to the context for nested steps
      context = %{context | map_arguments: context.map_arguments ++ map.arguments}

      with {:ok, sub_reactor} <- build_steps(sub_reactor, map, context) do
        arguments =
          map.arguments
          |> Enum.concat([Argument.from_template(:source, map.source)])

        step_options =
          map
          |> Map.take([:allow_async?, :batch_size, :return, :strict_ordering])
          |> Map.put(:state, :init)
          |> Map.put(:steps, sub_reactor.steps)
          |> Map.update!(:return, fn
            nil ->
              sub_reactor.steps
              |> List.first()
              |> Map.fetch!(:name)

            return ->
              return
          end)
          |> Enum.to_list()

        Builder.add_step(
          reactor,
          map.name,
          {Step.Map, step_options},
          arguments,
          description: map.description,
          guards: map.guards,
          max_retries: 0,
          ref: :step_name
        )
      end
    end

    @spec verify(Reactor.Dsl.Map.t(), any()) :: :ok | struct()
    def verify(step, dsl_state) do
      with :ok <- verify_at_least_one_step(step, dsl_state) do
        verify_return(step, dsl_state)
      end
    end

    defp verify_at_least_one_step(%{steps: [], name: name}, dsl_state) do
      DslError.exception(
        module: Verifier.get_persisted(dsl_state, :module),
        path: [:reactor, :map, name],
        message: "You must provide at least one child step to execute."
      )
    end

    defp verify_at_least_one_step(_step, _dsl_state), do: :ok

    defp verify_return(%{return: nil, steps: [_]}, _dsl_state), do: :ok

    defp verify_return(%{return: nil, name: name}, dsl_state),
      do:
        DslError.exception(
          module: Verifier.get_persisted(dsl_state, :module),
          path: [:reactor, :map, name],
          message:
            "You must specify which step to use as the return value when more than one nested step is present in a map."
        )

    defp verify_return(%{steps: steps, return: return, name: name}, dsl_state) do
      if Enum.any?(steps, &(&1.name == return)) do
        :ok
      else
        DslError.exception(
          module: Verifier.get_persisted(dsl_state, :module),
          path: [:reactor, :map, name],
          message:
            "The name `#{inspect(return)}` does not refer to a direct descendant of the `#{inspect(name)}` step."
        )
      end
    end

    defp build_steps(reactor, map, context) do
      map.steps
      |> reduce_while_ok(reactor, fn step, reactor ->
        build_step_with_context(step, reactor, context)
      end)
    end

    defp build_step_with_context(%Dsl.Compose{} = compose, reactor, context) do
      # For compose steps, add the map's arguments to the compose arguments
      # if they're not already present
      inherited_args = context.map_arguments
      explicit_arg_names = MapSet.new(compose.arguments, & &1.name)

      additional_args =
        inherited_args
        |> Enum.reject(fn arg -> MapSet.member?(explicit_arg_names, arg.name) end)

      compose_with_args = %{compose | arguments: compose.arguments ++ additional_args}
      Dsl.Build.build(compose_with_args, reactor)
    end

    defp build_step_with_context(%Dsl.Map{} = nested_map, reactor, context) do
      # For nested maps, pass the context down
      build(nested_map, reactor, context)
    end

    defp build_step_with_context(step, reactor, _context) do
      # For regular steps, use normal build
      Dsl.Build.build(step, reactor)
    end
  end
end
