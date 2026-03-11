# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.ManualRead do
  @moduledoc """
  A module to implement manual read actions.
  """

  @type context :: %{
          optional(:actor) => term,
          optional(:tenant) => term,
          optional(:authorize?) => boolean,
          optional(:domain) => module,
          optional(any) => any
        }

  @type extra_info :: %{
          optional(:full_count) => non_neg_integer()
        }

  @callback load_relationships(
              query :: Ash.Query.t(),
              results :: list(Ash.Resource.record()),
              opts :: Keyword.t(),
              context :: context(),
              lazy? :: boolean()
            ) ::
              {:ok, list(Ash.Resource.record())}
              | {:ok, list(Ash.Resource.record()), extra_info()}
              | {:error, term}

  @callback read(
              query :: Ash.Query.t(),
              data_layer_query :: term,
              opts :: Keyword.t(),
              context :: context()
            ) ::
              {:ok, list(Ash.Resource.record())}
              | {:ok, list(Ash.Resource.record()), extra_info()}
              | {:error, term}

  @optional_callbacks [
    load_relationships: 5
  ]

  @doc false
  @spec read(module(), Ash.Query.t(), term(), Keyword.t(), context()) ::
          {:ok, list(Ash.Resource.record())}
          | {:ok, list(Ash.Resource.record()), extra_info()}
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
          list(Ash.Resource.record()),
          Keyword.t(),
          context(),
          boolean()
        ) ::
          {:ok, list(Ash.Resource.record())}
          | {:ok, list(Ash.Resource.record()), extra_info()}
          | {:error, term()}
  def load_relationships(module, query, results, opts, context, lazy?) do
    result = module.load_relationships(query, results, opts, context, lazy?)

    if match?({:ok, _}, result) or match?({:ok, _, _}, result) or match?({:error, _}, result) do
      result
    else
      raise Ash.Error.Framework.InvalidReturnType,
        message: """
        Invalid value returned from #{inspect(module)}.load_relationships/5.

        The callback #{inspect(__MODULE__)}.load_relationships/5 expects {:ok, list}, {:ok, list, extra_info}, or {:error, term}.
        """
    end
  end

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
