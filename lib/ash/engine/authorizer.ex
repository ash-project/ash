defmodule Ash.Engine.Authorizer do
  @moduledoc """
  The interface for an ash authorizer

  These will typically be implemented by an extension, but a custom
  one can be implemented by defining a module that adopts this behaviour
  and using `@authorizers YourAuthorizer` to your resource.
  """
  @type state :: map
  @type context :: map
  @callback initial_state(Ash.resource(), Ash.actor(), Ash.action(), boolean) :: state
  @callback strict_check_context(state) :: [atom]
  @callback strict_check(state, context) ::
              :authorized
              | {:continue, state}
              | {:filter, Keyword.t()}
              | {:filter_and_continue, Keyword.t(), state}
              | {:error, Ash.error()}
  @callback check_context(state) :: [atom]
  @callback check(state, context) ::
              :authorized | {:data, list(Ash.record())} | {:error, Ash.error()}

  def initial_state(module, actor, resource, action, verbose?) do
    module.initial_state(actor, resource, action, verbose?)
  end

  def strict_check_context(module, state) do
    module.strict_check_context(state)
  end

  def strict_check(module, state, context) do
    module.strict_check(state, context)
  end

  def check_context(module, state) do
    module.check_context(state)
  end

  def check(module, state, context) do
    module.check(state, context)
  end
end
