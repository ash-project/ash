defmodule Ash.Reactor.Dsl.Transaction do
  @moduledoc """
  The `transaction` entity for the `Ash.Reactor` reactor extension.
  """

  # Comes from Ecto's default:
  # https://hexdocs.pm/ecto/Ecto.Repo.html#module-shared-options
  @default_timeout 15_000

  defstruct __identifier__: nil,
            arguments: [],
            description: nil,
            name: nil,
            resources: [],
            return: nil,
            steps: [],
            type: :transaction,
            timeout: @default_timeout,
            wait_for: []

  @type t :: %__MODULE__{
          __identifier__: any,
          arguments: [],
          description: nil | String.t(),
          name: atom,
          resources: [Ash.Resource.t()],
          return: atom,
          steps: [Reactor.Step.t()],
          timeout: timeout(),
          type: :transaction,
          wait_for: [Reactor.Dsl.WaitFor.t()]
        }

  @doc false
  def __entity__,
    do: %Spark.Dsl.Entity{
      name: :transaction,
      describe:
        "Creates a group of steps which will be executed inside a data layer transaction.",
      target: __MODULE__,
      args: [:name, :resources],
      identifier: :name,
      imports: [Reactor.Dsl.Argument],
      entities: [
        wait_for: [Reactor.Dsl.WaitFor.__entity__()],
        steps: []
      ],
      recursive_as: :steps,
      schema: [
        name: [
          type: :atom,
          required: true,
          doc: "A unique name for the step."
        ],
        resources: [
          type: {:wrap_list, {:spark, Ash.Resource}},
          required: true,
          doc: """
          A resource or list of resources to consider in the transaction.
          """
        ],
        return: [
          type: :atom,
          required: false,
          doc: """
          The name of the step whose result will be returned as the return value of the transaction.
          """
        ],
        timeout: [
          type: {:or, [:pos_integer, {:in, [:infinity]}]},
          required: false,
          default: @default_timeout,
          doc: """
          How long to allow the transaction to run before timing out.
          """
        ]
      ]
    }
end
