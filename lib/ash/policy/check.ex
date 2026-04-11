# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Policy.Check do
  @moduledoc """
  A behaviour for declaring checks, which can be used to easily construct
  authorization rules.

  If a check can be expressed simply, i.e as a function of the actor, or the context of the request,
  see `Ash.Policy.SimpleCheck` for an easy way to write that check.
  If a check can be expressed with a filter statement, see `Ash.Policy.FilterCheck`
  for an easy way to write that check.
  """

  @type actor :: any
  @type options :: Keyword.t()
  @type authorizer :: Ash.Policy.Authorizer.t()
  @type check_type :: :simple | :filter | :manual
  @type ref :: {module(), Keyword.t()} | module()

  defstruct [:check, :check_module, :check_opts, :type, :__spark_metadata__]

  @doc false
  def transform(%{check: {check_module, opts}} = policy) do
    case init(check_module, opts) do
      {:ok, opts} ->
        {:ok,
         %{policy | check: {check_module, opts}, check_module: check_module, check_opts: opts}}

      {:error, error} ->
        {:error, error}
    end
  end

  @type context() :: %{resource: Ash.Resource.t()}

  @type t :: %__MODULE__{
          check: ref(),
          check_module: module(),
          check_opts: options(),
          type: check_type(),
          __spark_metadata__: Spark.Dsl.Entity.spark_meta()
        }

  @doc """
  Strict checks should be cheap, and should never result in external calls (like database or domain)

  It should return `{:ok, true}` if it can tell that the request is authorized, and `{:ok, false}` if
  it can tell that it is not. If unsure, it should return `{:ok, :unknown}`
  """
  @callback strict_check(actor(), authorizer(), options()) ::
              {:ok, boolean | :unknown} | {:error, term}
  @doc """
  An optional callback, that allows the check to work with policies set to `access_type :filter`

  Return a keyword list filter that will be applied to the query being made, and will scope the results to match the rule
  """
  @callback auto_filter(actor(), authorizer(), options()) :: Keyword.t() | Ash.Expr.t()
  @doc """
  An optional callback, that allows the check to work with policies set to `access_type :runtime`

  Takes a list of records, and returns the subset of authorized records.
  """
  @callback check(actor(), list(Ash.Resource.record()), map, options) ::
              list(Ash.Resource.record())
  @doc "Describe the check in human readable format, given the options"
  @callback describe(options()) :: String.t()

  @doc "Expands the description of the check, given the actor and subject"
  @callback expand_description(
              actor(),
              authorizer(),
              options()
            ) :: {:ok, String.t()} | :none

  @doc "Whether or not the expanded description should replace the basic description in breakdowns"
  @callback prefer_expanded_description?() :: boolean()

  @doc "Whether or not your check requires the original data of a changeset (if applicable)"
  @callback requires_original_data?(authorizer(), options()) :: boolean()

  @doc """
  The type of the check

  `:manual` checks must be written by hand as standard check modules
  `:filter` checks can use `Ash.Policy.FilterCheck` for simplicity
  `:simple` checks can use `Ash.Policy.SimpleCheck` for simplicity
  """
  @callback type() :: check_type()
  @callback eager_evaluate?() :: boolean()

  @doc """
  Simplify a check reference into a SAT expression of simpler check references.

  This is used by the SAT solver to break down complex checks into simpler components
  for more efficient policy evaluation. For example, a check that matches multiple
  action types could be simplified into an OR expression of separate checks for each
  action type. Or ActorAbsent could simplify into `not(ActorPresent)`.
  """
  @callback simplify(ref(), context()) :: Crux.Expression.t(ref())

  @doc """
  Determine if the first check reference implies the second check reference.

  This is used by the SAT solver to optimize policy evaluation by understanding
  when one check being true guarantees another check is also true.
  """
  @callback implies?(ref(), ref(), context()) :: boolean()

  @doc """
  Determine if two check references are mutually exclusive (conflicting).

  This is used by the SAT solver to optimize policy evaluation by understanding
  when two checks cannot both be true at the same time.
  """
  @callback conflicts?(ref(), ref(), context()) :: boolean()

  @doc """
  Initialize and validate the opts for the check module.

  This callback is called at compile time when the check is defined in a policy.
  It allows checks to verify and normalize their options before they are used.
  """
  @callback init(opts :: Keyword.t()) :: {:ok, Keyword.t()} | {:error, String.t()}

  @optional_callbacks check: 4,
                      auto_filter: 3,
                      expand_description: 3,
                      simplify: 2,
                      implies?: 3,
                      conflicts?: 3

  require Ash.BehaviourHelpers

  @doc false
  @spec strict_check(module(), actor(), authorizer(), options()) ::
          {:ok, boolean() | :unknown} | {:error, term()}
  def strict_check(module, actor, authorizer, opts) do
    Ash.BehaviourHelpers.call_and_validate_return(
      module,
      :strict_check,
      [actor, authorizer, opts],
      [{:ok, true}, {:ok, false}, {:ok, :unknown}, {:error, :_}],
      behaviour: __MODULE__,
      callback_name: "strict_check/3"
    )
  end

  @doc false
  @spec check(module(), actor(), [Ash.Resource.record()], map(), options()) ::
          [Ash.Resource.record()]
  def check(module, actor, records, context, opts) do
    result = apply(module, :check, [actor, records, context, opts])

    if is_list(result) do
      result
    else
      raise Ash.Error.Framework.InvalidReturnType,
        message: """
        Invalid value returned from #{inspect(module)}.check/4.

        The callback #{inspect(__MODULE__)}.check/4 expects a list of records.
        """
    end
  end

  @doc false
  @spec auto_filter(module(), actor(), authorizer(), options()) ::
          Keyword.t() | Ash.Expr.t() | nil
  def auto_filter(module, actor, authorizer, opts) do
    result = apply(module, :auto_filter, [actor, authorizer, opts])

    if is_list(result) or is_struct(result) or is_tuple(result) or is_nil(result) or
         result == false do
      result
    else
      raise Ash.Error.Framework.InvalidReturnType,
        message: """
        Invalid value returned from #{inspect(module)}.auto_filter/3.

        The callback #{inspect(__MODULE__)}.auto_filter/3 expects a keyword list, Ash.Expr.t(), nil, or false.
        """
    end
  end

  @doc false
  @spec auto_filter_not(module(), actor(), authorizer(), options()) ::
          Keyword.t() | Ash.Expr.t() | nil
  def auto_filter_not(module, actor, authorizer, opts) do
    result = apply(module, :auto_filter_not, [actor, authorizer, opts])

    if is_list(result) or is_struct(result) or is_tuple(result) or is_nil(result) or
         result == false do
      result
    else
      raise Ash.Error.Framework.InvalidReturnType,
        message: """
        Invalid value returned from #{inspect(module)}.auto_filter_not/3.

        The callback #{inspect(__MODULE__)}.auto_filter_not/3 expects a keyword list, Ash.Expr.t(), nil, or false.
        """
    end
  end

  @doc false
  @spec describe(module(), options()) :: String.t()
  def describe(module, opts) do
    result = apply(module, :describe, [opts])

    if is_binary(result) do
      result
    else
      raise Ash.Error.Framework.InvalidReturnType,
        message: """
        Invalid value returned from #{inspect(module)}.describe/1.

        The callback #{inspect(__MODULE__)}.describe/1 expects a String.t().
        """
    end
  end

  @doc false
  @spec init(module(), Keyword.t()) :: {:ok, Keyword.t()} | {:error, String.t()}
  def init(module, opts) do
    Ash.BehaviourHelpers.call_and_validate_return(
      module,
      :init,
      [opts],
      [{:ok, :_}, {:error, :_}],
      behaviour: __MODULE__,
      callback_name: "init/1"
    )
  end

  @doc false
  @spec expand_description(module(), actor(), authorizer(), options()) ::
          {:ok, String.t()} | :none
  def expand_description(module, actor, authorizer, opts) do
    if function_exported?(module, :expand_description, 3) do
      Ash.BehaviourHelpers.call_and_validate_return(
        module,
        :expand_description,
        [actor, authorizer, opts],
        [{:ok, :_}, :none],
        behaviour: __MODULE__,
        callback_name: "expand_description/3"
      )
    else
      :none
    end
  end

  @doc false
  @spec requires_original_data?(module(), authorizer(), options()) :: boolean()
  def requires_original_data?(module, authorizer, opts) do
    Ash.BehaviourHelpers.call_and_validate_return(
      module,
      :requires_original_data?,
      [authorizer, opts],
      [true, false],
      behaviour: __MODULE__,
      callback_name: "requires_original_data?/2"
    )
  end

  @doc false
  @spec type(module()) :: check_type()
  def type(module) do
    Ash.BehaviourHelpers.call_and_validate_return(
      module,
      :type,
      [],
      [:simple, :filter, :manual],
      behaviour: __MODULE__,
      callback_name: "type/0"
    )
  end

  @doc false
  @spec simplify(module(), ref(), context()) :: Crux.Expression.t(ref())
  def simplify(module, ref, context) do
    if function_exported?(module, :simplify, 2) do
      result = apply(module, :simplify, [ref, context])

      if result != nil do
        result
      else
        raise Ash.Error.Framework.InvalidReturnType,
          message: """
          Invalid value returned from #{inspect(module)}.simplify/2.

          The callback #{inspect(__MODULE__)}.simplify/2 must not return nil.
          """
      end
    else
      ref
    end
  end

  @doc false
  @spec implies?(module(), ref(), ref(), context()) :: boolean()
  def implies?(module, left, right, context) do
    if function_exported?(module, :implies?, 3) do
      Ash.BehaviourHelpers.call_and_validate_return(
        module,
        :implies?,
        [left, right, context],
        [true, false],
        behaviour: __MODULE__,
        callback_name: "implies?/3"
      )
    else
      false
    end
  end

  @doc false
  @spec conflicts?(module(), ref(), ref(), context()) :: boolean()
  def conflicts?(module, left, right, context) do
    if function_exported?(module, :conflicts?, 3) do
      Ash.BehaviourHelpers.call_and_validate_return(
        module,
        :conflicts?,
        [left, right, context],
        [true, false],
        behaviour: __MODULE__,
        callback_name: "conflicts?/3"
      )
    else
      false
    end
  end

  @doc false
  @spec prefer_expanded_description?(module()) :: boolean()
  def prefer_expanded_description?(module) do
    Ash.BehaviourHelpers.call_and_validate_return(
      module,
      :prefer_expanded_description?,
      [],
      [true, false],
      behaviour: __MODULE__,
      callback_name: "prefer_expanded_description?/0"
    )
  end

  @doc false
  @spec eager_evaluate?(module()) :: boolean()
  def eager_evaluate?(module) do
    Ash.BehaviourHelpers.call_and_validate_return(
      module,
      :eager_evaluate?,
      [],
      [true, false],
      behaviour: __MODULE__,
      callback_name: "eager_evaluate?/0"
    )
  end

  def defines_check?(module) do
    :erlang.function_exported(module, :check, 4)
  end

  def defines_auto_filter?(module) do
    :erlang.function_exported(module, :auto_filter, 3)
  end

  defmacro __using__(_opts) do
    quote do
      @behaviour Ash.Policy.Check

      import Ash.Expr
      require Ash.Query

      def type, do: :manual
      def init(opts), do: {:ok, opts}
      def requires_original_data?(_, _), do: false
      def prefer_expanded_description?, do: false
      def eager_evaluate?, do: false
      def simplify(ref, _context), do: ref
      def implies?(_, _, _context), do: false
      def conflicts?(_, _, _context), do: false

      defoverridable type: 0,
                     init: 1,
                     requires_original_data?: 2,
                     prefer_expanded_description?: 0,
                     eager_evaluate?: 0,
                     simplify: 2,
                     implies?: 3,
                     conflicts?: 3
    end
  end
end
