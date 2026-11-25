# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Actions.Implementation do
  @moduledoc """
  An implementation of a [generic action](generic-actions.md).


  ### Example
  ```
  defmodule YourModule do
    use Ash.Resource.Actions.Implementation

    def run(input, opts, context) do
      {:ok, "Hello"}
    end
  end
  ```
  """

  @doc false
  @spec run(
          module(),
          Ash.ActionInput.t(),
          Keyword.t(),
          Ash.Resource.Actions.Implementation.Context.t()
        ) ::
          :ok
          | {:ok, term()}
          | {:ok, term(), [Ash.Notifier.Notification.t()]}
          | {:error, term()}
  def run(module, action_input, opts, context) do
    Ash.BehaviourHelpers.call_and_validate_return(
      module,
      :run,
      [action_input, opts, context],
      [:ok, {:ok, :_}, {:ok, :_, :_}, {:error, :_}],
      behaviour: __MODULE__,
      callback_name: "run/3"
    )
  end

  defmodule Context do
    @moduledoc "The context passed into generic action functions"

    defstruct [:actor, :tenant, :as_of, :authorize?, :domain, :tracer, source_context: %{}]

    @typedoc """
    Execution context passed to `c:Ash.Resource.Actions.Implementation.run/3`.

    Use `:actor`, `:tenant`, and `:authorize?` to inspect the caller's context.
    Pass `scope: context` when invoking a nested action to carry over the
    actor, tenant, and shared context.
    """
    @type t :: %__MODULE__{
            actor: term,
            tenant: term,
            as_of: DateTime.t() | :now | nil,
            tracer: atom | list(atom) | nil,
            source_context: map(),
            authorize?: boolean,
            domain: module
          }
  end

  @doc """
  Runs the generic action with its prepared input.

  Read declared arguments with `Ash.ActionInput.get_argument/2`. `opts`
  comes from the implementation configured in the resource's `run`
  declaration. `context` carries action information such as the actor and
  tenant. Return `:ok` for an action without a return value, `{:ok, value}`
  for an action with a declared return type, or `{:error, error}` on failure.
  Use `{:ok, value, notifications}` to return a result with notifications.
  """
  @callback run(Ash.ActionInput.t(), opts :: Keyword.t(), Context.t()) ::
              :ok
              | {:ok, term()}
              | {:ok, term(), [Ash.Notifier.Notification.t()]}
              | {:error, term()}

  @doc """
  Declares a generic action implementation.

  Implement `c:run/3` and register the module with `run` inside the action.
  To supply implementation options, use `run {MyApp.RunReport, format: :csv}`.
  The `use` macro takes no configuration options.

  See [Generic actions](/documentation/topics/actions/generic-actions.md).
  """
  defmacro __using__(_) do
    quote do
      @behaviour Ash.Resource.Actions.Implementation
    end
  end
end
