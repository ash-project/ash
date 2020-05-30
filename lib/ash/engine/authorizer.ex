defmodule Ash.Engine.Authorizer do
  @type state :: map
  @callback initial_state(Ash.resource(), Ash.actor(), Ash.action(), boolean) :: state
  @callback strict_check_context(state) :: [atom]
  @callback strict_check(state, map) ::
              :authorized
              | {:continue, state}
              | {:filter, Keyword.t()}
              | {:error, Ash.error()}
  @callback check_context(state) :: [atom]
  @callback check(state, list(Ash.record()), map) ::
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

  def check(module, state, data, context) do
    module.check(state, data, context)
  end
end
