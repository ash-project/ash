# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
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

  require Ash.BehaviourHelpers

  def run(module, action_input, opts, context) do
    Ash.BehaviourHelpers.check_type!(module, module.run(action_input, opts, context), [
      :ok,
      {:ok, _},
      {:ok, _, list} when is_list(list),
      {:error, _}
    ])
  end

  defmodule Context do
    @moduledoc "The context passed into generic action functions"

    defstruct [:actor, :tenant, :authorize?, :domain, :tracer, source_context: %{}]

    @type t :: %__MODULE__{
            actor: term,
            tenant: term,
            tracer: atom | list(atom) | nil,
            source_context: map(),
            authorize?: boolean,
            domain: module
          }
  end

  @callback run(Ash.ActionInput.t(), opts :: Keyword.t(), Context.t()) ::
              :ok
              | {:ok, term()}
              | {:ok, term(), [Ash.Notifier.Notification.t()]}
              | {:error, term()}

  defmacro __using__(_) do
    quote do
      @behaviour Ash.Resource.Actions.Implementation
    end
  end
end
