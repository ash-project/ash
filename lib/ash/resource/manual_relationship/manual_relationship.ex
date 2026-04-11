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

  @callback select(opts :: Keyword.t()) :: list(atom)

  @callback load(
              list(Ash.Resource.record()),
              opts :: Keyword.t(),
              context :: Context.t()
            ) ::
              {:ok, [term] | map} | {:error, term}

  @doc false
  @spec load(module(), [Ash.Resource.record()], Keyword.t(), Context.t()) ::
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

  defmacro __using__(_) do
    quote do
      @behaviour Ash.Resource.ManualRelationship

      def select(_opts), do: []

      defoverridable select: 1
    end
  end
end
