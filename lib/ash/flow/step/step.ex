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
        doc: "The name of the step. Will be used when expressing dependencies, and step inputs."
      ],
      wait_for: [
        type: :any,
        doc: """
        Ensures that the step happens after the configured step or steps.

        This value is just a template that isn't used, except to determine dependencies, so you can
        use it like this `wait_for [result(:step_one), result(:step_two)]` or `wait_for result(:step)`.
        """
      ],
      touches_resources: [
        type: {:list, :atom},
        doc: """
        A list of resources touched by any custom logic in this step. This is used in the case that this step is run in a transaction. This is primarily only needed for `custom` steps.
        """
      ],
      description: [
        type: :string,
        doc: """
        A description for the step.
        """
      ]
    ]
  end

  @doc false
  def shared_action_opts do
    [
      resource: [
        type: :atom,
        required: true,
        doc: "The resource to call the action on."
      ],
      action: [
        type: :atom,
        required: true,
        doc: "The action to call on the resource."
      ],
      api: [
        type: :atom,
        doc:
          "The api to use when calling the action. Defaults to the api set in the `flow` section."
      ],
      tenant: [
        type: :any,
        doc: "A tenant to use for the operation. May be a template or a literal value."
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

      Available template functions:

      - `arg/1` to refer to a flow argument
      - `result/1` to refer to the result of another step
      """
    ]
  end
end
