defmodule Ash.Engine.Authorizer do
  @type state :: map
  @callback initial_state(Ash.resource(), Ash.actor(), Ash.action(), boolean) :: state
  @callback strict_check_context(state) :: [atom]
  @callback strict_check(state, map) :: :authorized | {:continue, map} | {:error, Ash.error()}
  @callback check_context(state) :: [atom]
  @callback check(state, map) :: :authorized | {:error, Ash.error()}

  def initial_state(module, actor, resource, action, verbose?) do
    module.initial_state(actor, resource, action, verbose?)
  end

  def strict_check_context(module, state) do
    module.strict_check_context(state)
  end

  @spec strict_check(atom, any, any) :: any
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
