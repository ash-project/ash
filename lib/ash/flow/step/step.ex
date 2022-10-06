defmodule Ash.Flow.Step do
  @moduledoc """
  A behaviour for implementing a custom step in a flow.
  """

  @callback run(input :: map | nil, opts :: Keyword.t(), context :: map) ::
              {:ok, term}
              | {:ok, term, %{optional(:notifications) => list(Ash.Notifier.Notification.t())}}
              | {:error, term}

  @callback describe(opts :: Keyword.t()) :: String.t()
  @callback short_name(opts :: Keyword.t()) :: String.t()
  @optional_callbacks [describe: 1, short_name: 1]

  defmacro __using__(_) do
    quote do
      @behaviour Ash.Flow.Step
    end
  end

  @doc false
  def shared_opts do
    [
      name: [
        type: :atom,
        required: true,
        doc: "The name of the step. Will be used when expressing dependencies, and step inputs.",
        links: []
      ],
      wait_for: [
        type: :any,
        doc: """
        Ensures that the step happens after the configured step or steps.

        This value is just a template that isn't used, except to determine dependencies, so you can
        use it like this `wait_for [result(:step_one), result(:step_two)]` or `wait_for result(:step)`.
        """,
        links: []
      ],
      touches_resources: [
        type: {:list, :atom},
        doc: """
        A list of resources touched by any custom logic in this step. This is used in the case that this step is run in a transaction. This is primarily only needed for `custom` steps.
        """,
        links: []
      ],
      halt_if: [
        type: :any,
        doc: """
        Halts the step by emitting an error (with an `Ash.Error.Flow.Halted`). Can use template variables.

        To attach a specific reason, use a `halt_reason`.

        If you need more complex halting logic, then you'd want to use a custom step, and return `{:error, Ash.Error.Flow.Halted.exception(...)}`
        """
      ],
      halt_reason: [
        type: :any,
        doc: """
        Configures the reason for the `halt_if` clause.
        """,
        default: :halted
      ],
      description: [
        type: :string,
        doc: """
        A description for the step.
        """,
        links: []
      ]
    ]
  end

  @doc false
  def shared_action_opts do
    [
      resource: [
        type: :atom,
        required: true,
        doc: "The resource to call the action on.",
        links: []
      ],
      action: [
        type: :atom,
        required: true,
        doc: "The action to call on the resource.",
        links: []
      ],
      api: [
        type: :atom,
        doc:
          "The api to use when calling the action. Defaults to the api set in the `flow` section.",
        links: []
      ],
      tenant: [
        type: :any,
        doc: "A tenant to use for the operation. May be a template or a literal value.",
        links: []
      ],
      input: input()
    ]
  end

  @doc false
  def input do
    [
      type: :any,
      doc: """
      A template for the input.
      """,
      links: []
    ]
  end
end
