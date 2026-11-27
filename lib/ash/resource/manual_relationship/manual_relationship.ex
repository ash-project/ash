# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.ManualRelationship do
  @moduledoc """
  A module to implement manual relationships.
  """

  defmodule Context do
    @moduledoc "The context passed into manual relationship functions"
    defstruct [
      :relationship,
      :query,
      :actor,
      :tenant,
      :tracer,
      :authorize?,
      :domain,
      source_context: %{}
    ]

    @typedoc """
    Context passed to the manual relationship loader. `:query` is the query
    for the destination records and `:relationship` identifies the relationship
    being loaded. The context can also be used as an `Ash.Scope`.
    """
    @type t :: %__MODULE__{
            relationship: Ash.Resource.Relationships.relationship(),
            query: Ash.Query.t(),
            actor: term,
            tracer: atom | list(atom) | nil,
            tenant: term,
            source_context: map(),
            authorize?: boolean,
            domain: module
          }
  end

  @doc """
  Lists source attributes needed by the loader.

  Return the attribute names the implementation reads from the source
  records, or `:*` to select all source attributes. The default is `[]`.
  """
  @callback select(opts :: Keyword.t()) :: list(atom) | :*

  @doc """
  Loads related data for a batch of source records.

  Return `{:ok, results}` with one result per input record in the same
  order. A to-many result is a list of related records and a to-one result
  is a related record or `nil`. A map keyed by source record ID is also
  supported. Return `{:error, error}` on failure.

  Use `context.query` to honor the caller's requested filters and sort.
  When reading through Ash, pass `scope: context` to carry the actor,
  tenant, and shared context to the read.
  """
  @callback load(
              list(Ash.Resource.Record.t()),
              opts :: Keyword.t(),
              context :: Context.t()
            ) ::
              {:ok, [term] | map} | {:error, term}

  @doc false
  @spec load(module(), [Ash.Resource.Record.t()], Keyword.t(), Context.t()) ::
          {:ok, [term()] | map()} | {:error, term()}
  def load(module, records, opts, context) do
    result = apply(module, :load, [records, opts, context])

    case result do
      {:ok, _} ->
        result

      {:error, _} ->
        result

      _ ->
        raise Ash.Error.Framework.InvalidReturnType,
          message: """
          Invalid value returned from #{inspect(module)}.load/3.

          The callback #{inspect(__MODULE__)}.load/3 expects {:ok, [term()] | map()} or {:error, term()}.
          """
    end
  end

  @doc """
  Declares the module as a manual relationship loader.

  Implement `c:load/3` and register the module with `manual` inside the
  relationship. Override `c:select/1` to request additional source fields.
  The `use` macro takes no configuration options.

  See [Manual relationships](/documentation/topics/resources/relationships.md#manual-relationships).
  """
  defmacro __using__(_) do
    quote do
      @behaviour Ash.Resource.ManualRelationship

      def select(_opts), do: []

      defoverridable select: 1
    end
  end
end
