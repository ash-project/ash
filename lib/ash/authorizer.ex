defmodule Ash.Authorizer do
  @moduledoc """
  The interface for an ash authorizer

  These will typically be implemented by an extension, but a custom
  one can be implemented by defining an extension that also adopts this behaviour.

  Then you can extend a resource with `authorizers: [YourAuthorizer]`
  """
  @type state :: map
  @type context :: map
  @callback initial_state(
              Ash.Resource.t(),
              Ash.Resource.record(),
              Ash.Resource.Actions.action(),
              Ash.Domain.t()
            ) :: state
  @callback strict_check_context(state) :: [atom]
  @callback strict_check(state, context) ::
              {:authorized, state}
              | {:continue, state}
              | {:filter, Keyword.t()}
              | {:filter, Keyword.t(), state}
              | {:filter_and_continue, Keyword.t(), state}
              | {:error, term}
  @callback alter_filter(filter :: Ash.Filter.t(), state, context) ::
              {:ok, Ash.Filter.t()} | {:error, Ash.Error.t()}
  @callback add_calculations(Ash.Query.t() | Ash.Changeset.t(), state, context) ::
              {:ok, Ash.Query.t() | Ash.Changeset.t(), state} | {:error, Ash.Error.t()}
  @callback alter_results(state, list(Ash.Resource.record()), context) ::
              {:ok, list(Ash.Resource.record())} | {:error, Ash.Error.t()}
  @callback check_context(state) :: [atom]
  @callback check(state, context) ::
              :authorized | {:data, list(Ash.Resource.record())} | {:error, term}
  @callback exception(atom, state) :: no_return

  @optional_callbacks [exception: 2, add_calculations: 3, alter_results: 3, alter_filter: 3]

  def initial_state(module, actor, resource, action, domain) do
    module.initial_state(actor, resource, action, domain)
  end

  def exception(module, reason, state) do
    if function_exported?(module, :exception, 2) do
      module.exception(reason, state)
    else
      if reason == :must_pass_strict_check do
        Ash.Error.Forbidden.MustPassStrictCheck.exception([])
      else
        Ash.Error.Forbidden.exception([])
      end
    end
  end

  def strict_check_context(module, state) do
    Enum.uniq(module.strict_check_context(state) ++ [:query, :changeset])
  end

  def strict_check(module, state, context) do
    module.strict_check(state, context)
  end

  def add_calculations(module, query_or_changeset, state, context) do
    if function_exported?(module, :add_calculations, 3) do
      module.add_calculations(query_or_changeset, state, context)
    else
      {:ok, query_or_changeset, state}
    end
  end

  def alter_results(module, state, records, context) do
    if function_exported?(module, :alter_results, 3) do
      module.alter_results(state, records, context)
    else
      {:ok, records}
    end
  end

  def alter_filter(module, state, filter, context) do
    if function_exported?(module, :alter_filter, 3) do
      module.alter_filter(filter, state, context)
    else
      {:ok, filter}
    end
  end

  def alter_sort(module, state, sort, context) do
    if function_exported?(module, :alter_sort, 3) do
      module.alter_sort(sort, state, context)
    else
      {:ok, sort}
    end
  end

  def check_context(module, state) do
    module.check_context(state)
  end

  def check(module, state, context) do
    module.check(state, context)
  end
end
