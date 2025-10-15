# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
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
    {:ok, %{policy | check_module: check_module, check_opts: opts}}
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

  @optional_callbacks check: 4,
                      auto_filter: 3,
                      expand_description: 3,
                      simplify: 2,
                      implies?: 3,
                      conflicts?: 3

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
      def requires_original_data?(_, _), do: false
      def prefer_expanded_description?, do: false
      def eager_evaluate?, do: false
      def simplify(ref, _context), do: ref
      def implies?(_, _, _context), do: false
      def conflicts?(_, _, _context), do: false

      defoverridable type: 0,
                     requires_original_data?: 2,
                     prefer_expanded_description?: 0,
                     eager_evaluate?: 0,
                     simplify: 2,
                     implies?: 3,
                     conflicts?: 3
    end
  end
end
