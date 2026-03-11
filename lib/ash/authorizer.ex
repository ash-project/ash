# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Authorizer do
  @moduledoc """
  The interface for an ash authorizer

  These will typically be implemented by an extension, but a custom
  one can be implemented by defining an extension that also adopts this behaviour.

  Then you can extend a resource with `authorizers: [YourAuthorizer]`
  """
  require Ash.BehaviourHelpers

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
              :authorized
              | {:data, list(Ash.Resource.record())}
              | {:error, :forbidden, state}
              | {:error, Ash.Error.t()}
  @callback exception(atom, state) :: no_return

  @optional_callbacks [exception: 2, add_calculations: 3, alter_results: 3, alter_filter: 3]

  defmacro __using__(_) do
    quote do
      @behaviour Ash.Authorizer
    end
  end

  @doc false
  @spec initial_state(
          module(),
          Ash.Resource.record(),
          Ash.Resource.t(),
          Ash.Resource.Actions.action(),
          Ash.Domain.t()
        ) :: state
  def initial_state(module, actor, resource, action, domain) do
    result = module.initial_state(actor, resource, action, domain)

    if is_map(result) do
      result
    else
      raise Ash.Error.Framework.InvalidReturnType,
        message: """
        Invalid value returned from #{inspect(module)}.initial_state/4.

        The callback #{inspect(__MODULE__)}.initial_state/4 expects a map (state).
        """
    end
  end

  @doc false
  @spec exception(module(), atom(), state()) :: no_return
  def exception(module, reason, state) do
    if function_exported?(module, :exception, 2) do
      module.exception(reason, state)
    else
      if reason == :must_pass_strict_check do
        raise Ash.Error.Forbidden.MustPassStrictCheck.exception([])
      else
        raise Ash.Error.Forbidden.exception([])
      end
    end
  end

  @doc false
  @spec strict_check_context(module(), state()) :: [atom()]
  def strict_check_context(module, state) do
    result = module.strict_check_context(state)

    if is_list(result) and Enum.all?(result, &is_atom/1) do
      Enum.uniq(result ++ [:query, :changeset])
    else
      raise Ash.Error.Framework.InvalidReturnType,
        message: """
        Invalid value returned from #{inspect(module)}.strict_check_context/1.

        The callback #{inspect(__MODULE__)}.strict_check_context/1 expects a list of atoms.
        """
    end
  end

  @doc false
  @spec strict_check(module(), state(), context()) ::
          {:authorized, state()}
          | {:continue, state()}
          | {:filter, Keyword.t()}
          | {:filter, Keyword.t(), state()}
          | {:filter_and_continue, Keyword.t(), state()}
          | {:error, term()}
  def strict_check(module, state, context) do
    Ash.BehaviourHelpers.call_and_validate_return(
      module,
      :strict_check,
      [state, context],
      [
        {:authorized, :_},
        {:continue, :_},
        {:filter, :_},
        {:filter, :_, :_},
        {:filter_and_continue, :_, :_},
        {:error, :_}
      ],
      behaviour: __MODULE__,
      callback_name: "strict_check/2"
    )
  end

  @doc false
  @spec add_calculations(module(), Ash.Query.t() | Ash.Changeset.t(), state(), context()) ::
          {:ok, Ash.Query.t() | Ash.Changeset.t(), state()} | {:error, Ash.Error.t()}
  def add_calculations(module, query_or_changeset, state, context) do
    if function_exported?(module, :add_calculations, 3) do
      Ash.BehaviourHelpers.call_and_validate_return(
        module,
        :add_calculations,
        [query_or_changeset, state, context],
        [{:ok, :_, :_}, {:error, :_}],
        behaviour: __MODULE__,
        callback_name: "add_calculations/3"
      )
    else
      {:ok, query_or_changeset, state}
    end
  end

  @doc false
  @spec alter_results(module(), state(), [Ash.Resource.record()], context()) ::
          {:ok, [Ash.Resource.record()]} | {:error, Ash.Error.t()}
  def alter_results(module, state, records, context) do
    if function_exported?(module, :alter_results, 3) do
      Ash.BehaviourHelpers.call_and_validate_return(
        module,
        :alter_results,
        [state, records, context],
        [{:ok, :_}, {:error, :_}],
        behaviour: __MODULE__,
        callback_name: "alter_results/3"
      )
    else
      {:ok, records}
    end
  end

  @doc false
  @spec alter_filter(module(), state(), Ash.Filter.t(), context()) ::
          {:ok, Ash.Filter.t()} | {:error, Ash.Error.t()}
  def alter_filter(module, state, filter, context) do
    if function_exported?(module, :alter_filter, 3) do
      Ash.BehaviourHelpers.call_and_validate_return(
        module,
        :alter_filter,
        [filter, state, context],
        [{:ok, :_}, {:error, :_}],
        behaviour: __MODULE__,
        callback_name: "alter_filter/3"
      )
    else
      {:ok, filter}
    end
  end

  @doc false
  @spec alter_sort(module(), state(), term(), context()) ::
          {:ok, term()} | {:error, Ash.Error.t()}
  def alter_sort(module, state, sort, context) do
    if function_exported?(module, :alter_sort, 3) do
      result = module.alter_sort(sort, state, context)

      if match?({:ok, _}, result) or match?({:error, _}, result) do
        result
      else
        raise Ash.Error.Framework.InvalidReturnType,
          message: """
          Invalid value returned from #{inspect(module)}.alter_sort/3.

          The callback expects {:ok, sort} or {:error, Ash.Error.t()}.
          """
      end
    else
      {:ok, sort}
    end
  end

  @doc false
  @spec check_context(module(), state()) :: [atom()]
  def check_context(module, state) do
    result = module.check_context(state)

    if is_list(result) and Enum.all?(result, &is_atom/1) do
      result
    else
      raise Ash.Error.Framework.InvalidReturnType,
        message: """
        Invalid value returned from #{inspect(module)}.check_context/1.

        The callback #{inspect(__MODULE__)}.check_context/1 expects a list of atoms.
        """
    end
  end

  @doc false
  @spec check(module(), state(), context()) ::
          :authorized
          | {:data, [Ash.Resource.record()]}
          | {:error, :forbidden, state()}
          | {:error, Ash.Error.t()}
  def check(module, state, context) do
    Ash.BehaviourHelpers.call_and_validate_return(
      module,
      :check,
      [state, context],
      [
        :authorized,
        {:data, :_},
        {:error, :forbidden, :_},
        {:error, :_}
      ],
      behaviour: __MODULE__,
      callback_name: "check/2"
    )
  end
end
