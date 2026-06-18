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

  @type state :: map
  @type context :: map
  @callback initial_state(
              Ash.Resource.t(),
              Ash.Resource.Record.t(),
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
  @callback alter_results(state, list(Ash.Resource.Record.t()), context) ::
              {:ok, list(Ash.Resource.Record.t())} | {:error, Ash.Error.t()}
  @doc """
  Apply field-level authorization to a list of records that already have
  values populated in memory, without re-fetching them through a read.

  Implementations should walk each record and substitute `%Ash.ForbiddenField{}`
  for any field the actor isn't allowed to see (typically via field policies).

  This is the lightweight counterpart to `add_calculations/3` + the read
  pipeline's field-policy scrubbing — used when you have records in hand and
  just need to enforce field-level visibility without running a load.
  """
  @callback apply_field_level_auth(
              resource :: Ash.Resource.t(),
              records :: list(Ash.Resource.Record.t()),
              opts :: Keyword.t()
            ) :: {:ok, list(Ash.Resource.Record.t())} | {:error, Ash.Error.t()}
  @callback check_context(state) :: [atom]
  @callback check(state, context) ::
              :authorized
              | {:data, list(Ash.Resource.Record.t())}
              | {:error, :forbidden, state}
              | {:error, Ash.Error.t()}
  @doc """
  Returns the fields this authorizer may protect at field level for the resource.

  This is metadata for callers that need to know which fields can be hidden by
  authorization without running an action. Authorizers that do not implement this
  callback are treated as protecting no fields.
  """
  @callback protected_fields(Ash.Resource.t()) :: [atom()]
  @callback exception(atom, state) :: Exception.t()

  @optional_callbacks [
    exception: 2,
    add_calculations: 3,
    alter_results: 3,
    alter_filter: 3,
    apply_field_level_auth: 3,
    protected_fields: 1
  ]

  defmacro __using__(_) do
    quote do
      @behaviour Ash.Authorizer
    end
  end

  @doc false
  @spec initial_state(
          module(),
          Ash.Resource.Record.t(),
          Ash.Resource.t(),
          Ash.Resource.Actions.action(),
          Ash.Domain.t()
        ) :: state
  def initial_state(module, actor, resource, action, domain) do
    result = apply(module, :initial_state, [actor, resource, action, domain])

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
  @spec exception(module(), atom(), state()) :: Exception.t()
  def exception(module, reason, state) do
    if function_exported?(module, :exception, 2) do
      apply(module, :exception, [reason, state])
    else
      if reason == :must_pass_strict_check do
        Ash.Error.Forbidden.MustPassStrictCheck.exception([])
      else
        Ash.Error.Forbidden.exception([])
      end
    end
  end

  @doc false
  @spec strict_check_context(module(), state()) :: [atom()]
  def strict_check_context(module, state) do
    result = apply(module, :strict_check_context, [state])

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
  @spec alter_results(module(), state(), [Ash.Resource.Record.t()], context()) ::
          {:ok, [Ash.Resource.Record.t()]} | {:error, Ash.Error.t()}
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
      Ash.BehaviourHelpers.call_and_validate_return(
        module,
        :alter_sort,
        [sort, state, context],
        [{:ok, :_}, {:error, :_}],
        behaviour: __MODULE__,
        callback_name: "alter_sort/3"
      )
    else
      {:ok, sort}
    end
  end

  @doc false
  @spec check_context(module(), state()) :: [atom()]
  def check_context(module, state) do
    result = apply(module, :check_context, [state])

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
          | {:data, [Ash.Resource.Record.t()]}
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

  @doc false
  @spec protected_fields(module(), Ash.Resource.t()) :: [atom()]
  def protected_fields(module, resource) do
    if function_exported?(module, :protected_fields, 1) do
      result = apply(module, :protected_fields, [resource])

      if is_list(result) and Enum.all?(result, &is_atom/1) do
        Enum.uniq(result)
      else
        raise Ash.Error.Framework.InvalidReturnType,
          message: """
          Invalid value returned from #{inspect(module)}.protected_fields/1.

          The callback #{inspect(__MODULE__)}.protected_fields/1 expects a list of atoms.
          """
      end
    else
      []
    end
  end

  @doc """
  Apply field-level authorization to records that are already in memory,
  scrubbing any fields the actor isn't allowed to see.

  Walks each authorizer configured on the resource and invokes its
  `apply_field_level_auth/3` callback if defined. Returns the records with
  forbidden fields replaced by `%Ash.ForbiddenField{}`.

  Use this when you have records in hand (for example, returned from a
  generic action or constructed in memory) and want field policies applied
  without driving the records through a full read.

  Supported options:

  - `:actor` - the actor whose visibility is being checked.
  - `:tenant` - the tenant the records belong to.
  - `:domain` - the domain context.
  """
  @spec apply_field_level_auth(
          Ash.Resource.t(),
          Ash.Resource.Record.t() | [Ash.Resource.Record.t()],
          Keyword.t()
        ) ::
          {:ok, Ash.Resource.Record.t() | [Ash.Resource.Record.t()]} | {:error, Ash.Error.t()}
  def apply_field_level_auth(resource, records, opts \\ []) do
    {records, single?} =
      case records do
        list when is_list(list) -> {list, false}
        record -> {[record], true}
      end

    resource
    |> Ash.Resource.Info.authorizers()
    |> Enum.reduce_while({:ok, records}, fn module, {:ok, records} ->
      if function_exported?(module, :apply_field_level_auth, 3) do
        case Ash.BehaviourHelpers.call_and_validate_return(
               module,
               :apply_field_level_auth,
               [resource, records, opts],
               [{:ok, :_}, {:error, :_}],
               behaviour: __MODULE__,
               callback_name: "apply_field_level_auth/3"
             ) do
          {:ok, records} -> {:cont, {:ok, records}}
          {:error, error} -> {:halt, {:error, error}}
        end
      else
        {:cont, {:ok, records}}
      end
    end)
    |> case do
      {:ok, [single]} when single? -> {:ok, single}
      {:ok, records} -> {:ok, records}
      {:error, error} -> {:error, error}
    end
  end
end
