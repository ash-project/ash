defmodule Ash.Reactor.Dsl.AshStep do
  @moduledoc """
  The ash_step DSL module.

  See `d:Reactor.step`.
  """

  defstruct __identifier__: nil,
            arguments: [],
            async?: true,
            compensate: nil,
            impl: nil,
            max_retries: :infinity,
            name: nil,
            run: nil,
            transform: nil,
            undo: nil

  alias Reactor.{Dsl, Step}

  @type t :: %__MODULE__{
          arguments: [Dsl.Argument.t()],
          async?: boolean,
          compensate:
            nil | (any, Reactor.inputs(), Reactor.context() -> :ok | :retry | {:continue, any}),
          impl: nil | module | {module, keyword},
          max_retries: non_neg_integer() | :infinity,
          name: atom,
          run:
            nil
            | (Reactor.inputs(), Reactor.context() ->
                 {:ok, any} | {:ok, any, [Step.t()]} | {:halt | :error, any}),
          transform: nil | (any -> any),
          undo: nil | (any, Reactor.inputs(), Reactor.context() -> :ok | :retry | {:error, any}),
          __identifier__: any
        }

  @doc false
  def __entity__,
    do: %Spark.Dsl.Entity{
      name: :ash_step,
      describe: """
      Specifies a Ash.Reactor step.

      This is basically a wrapper around `Reactor.step`, in order to handle
      any returned notifications from the run step/function.

      See the `Reactor.Step` behaviour for more information.
      """,
      examples: [
        """
        ash_step :create_post, MyApp.CreatePostStep do
          argument :title, input(:title)
        end
        """,
        """
        ash_step :create_post do
          argument :title, input(:title)

          run fn %{title: title}, _ ->
            MyApp.Post.create(title, return_notifications?: true)
          end
        end
        """
      ],
      args: [:name, {:optional, :impl}],
      target: __MODULE__,
      identifier: :name,
      no_depend_modules: [:impl],
      entities: [arguments: [Dsl.Argument.__entity__(), Dsl.WaitFor.__entity__()]],
      recursive_as: :steps,
      schema: [
        name: [
          type: :atom,
          required: true,
          doc: """
          A unique name for the step. Used when choosing the return value of the Reactor and for arguments into other steps.
          """
        ],
        impl: [
          type: {:or, [{:spark_behaviour, Step}, nil]},
          required: false,
          doc: """
          A module that implements the `Reactor.Step` behaviour that provides the implementation.
          """
        ],
        run: [
          type: {:or, [{:mfa_or_fun, 1}, {:mfa_or_fun, 2}]},
          required: false,
          doc: """
          Provide an anonymous function which implements the `run/3` callback. Cannot be provided at the same time as the `impl` argument.
          """
        ],
        undo: [
          type: {:or, [{:mfa_or_fun, 1}, {:mfa_or_fun, 2}, {:mfa_or_fun, 3}]},
          required: false,
          doc: """
          Provide an anonymous function which implements the `undo/4` callback. Cannot be provided at the same time as the `impl` argument.
          """
        ],
        compensate: [
          type: {:or, [{:mfa_or_fun, 1}, {:mfa_or_fun, 2}, {:mfa_or_fun, 3}]},
          required: false,
          doc: """
          Provide an anonymous function which implements the `undo/4` callback. Cannot be provided at the same time as the `impl` argument.
          """
        ],
        max_retries: [
          type: {:or, [{:in, [:infinity]}, :non_neg_integer]},
          required: false,
          default: :infinity,
          doc: """
          The maximum number of times that the step can be retried before failing. Only used when the result of the `compensate/4` callback is `:retry`.
          """
        ],
        async?: [
          type: :boolean,
          required: false,
          default: true,
          doc: """
          When set to true the step will be executed asynchronously via Reactor's `TaskSupervisor`.
          """
        ],
        transform: [
          type: {:or, [{:spark_function_behaviour, Step, {Step.TransformAll, 1}}, nil]},
          required: false,
          default: nil,
          doc: """
          An optional transformation function which can be used to modify the entire argument map before it is passed to the step.
          """
        ]
      ]
    }
end
