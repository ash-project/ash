# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.ManualRead do
  @moduledoc """
  A module to implement manual read actions.
  """

  @typedoc """
  The actor, tenant, authorization setting, and domain for the manual read.
  """
  @type context :: %{
          optional(:actor) => term,
          optional(:tenant) => term,
          optional(:authorize?) => boolean,
          optional(:domain) => module,
          optional(any) => any
        }

  @typedoc """
  Additional information to return with read results.

  Set `:full_count` to the total number of matching records when providing
  the count for pagination.
  """
  @type extra_info :: %{
          optional(:full_count) => non_neg_integer()
        }

  @doc """
  Loads the relationships requested by the query on records already read.

  This callback is optional. Ash uses its standard relationship loader
  when omitted. If `lazy?` is true, preserve values already loaded where
  applicable. Return the records with their requested relationships loaded.
  """
  @callback load_relationships(
              query :: Ash.Query.t(),
              results :: list(Ash.Resource.Record.t()),
              opts :: Keyword.t(),
              context :: context(),
              lazy? :: boolean()
            ) ::
              {:ok, list(Ash.Resource.Record.t())}
              | {:ok, list(Ash.Resource.Record.t()), extra_info()}
              | {:error, term}

  @doc """
  Runs the manual read and returns a list of resource records.

  The Ash query contains the requested filters, sort, and pagination. The
  integrated data layer also provides its own query representation when
  applicable. Use `Ash.Query.apply_to/3` if you need to apply a query to
  records held in memory. Return `{:ok, records}`, `{:ok, records, extra_info}`
  to supply a count, or `{:error, error}` on failure.
  """
  @callback read(
              query :: Ash.Query.t(),
              data_layer_query :: term,
              opts :: Keyword.t(),
              context :: context()
            ) ::
              {:ok, list(Ash.Resource.Record.t())}
              | {:ok, list(Ash.Resource.Record.t()), extra_info()}
              | {:error, term}

  @optional_callbacks [
    load_relationships: 5
  ]

  @doc false
  @spec read(module(), Ash.Query.t(), term(), Keyword.t(), context()) ::
          {:ok, list(Ash.Resource.Record.t())}
          | {:ok, list(Ash.Resource.Record.t()), extra_info()}
          | {:error, term()}
  def read(module, query, data_layer_query, opts, context) do
    Ash.BehaviourHelpers.call_and_validate_return(
      module,
      :read,
      [query, data_layer_query, opts, context],
      [{:ok, :_}, {:ok, :_, :_}, {:error, :_}],
      behaviour: __MODULE__,
      callback_name: "read/4"
    )
  end

  @doc false
  @spec load_relationships(
          module(),
          Ash.Query.t(),
          list(Ash.Resource.Record.t()),
          Keyword.t(),
          context(),
          boolean()
        ) ::
          {:ok, list(Ash.Resource.Record.t())}
          | {:ok, list(Ash.Resource.Record.t()), extra_info()}
          | {:error, term()}
  def load_relationships(module, query, results, opts, context, lazy?) do
    Ash.BehaviourHelpers.call_and_validate_return(
      module,
      :load_relationships,
      [query, results, opts, context, lazy?],
      [{:ok, :_}, {:ok, :_, :_}, {:error, :_}],
      behaviour: __MODULE__,
      callback_name: "load_relationships/5"
    )
  end

  @doc """
  Declares the module as a manual read action implementation.

  Implement `c:read/4` and register the module in the resource's read action
  with `manual MyApp.CustomRead`. The `use` macro takes no configuration
  options. Supply callback options with `manual {MyApp.CustomRead, opts}`.

  See [Manual actions](/documentation/topics/actions/manual-actions.md).
  """
  defmacro __using__(_) do
    quote do
      @behaviour Ash.Resource.ManualRead
      @before_compile Ash.Resource.ManualRead
    end
  end

  defmacro __before_compile__(_) do
    quote do
      if Module.defines?(__MODULE__, {:load_relationships, 5}, :def) do
        def has_load_relationships?, do: true
      else
        def has_load_relationships?, do: false
      end
    end
  end
end
