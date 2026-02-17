# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Dsl.Middleware do
  @moduledoc """
  The `middleware` DSL entity struct.

  See `d:Reactor.middleware.middleware`.
  """

  alias Reactor.{Dsl.Build, Middleware}

  defstruct __identifier__: nil, description: nil, module: nil, __spark_metadata__: nil

  @type t :: %__MODULE__{
          __identifier__: any,
          description: nil | String.t(),
          module: Middleware.t(),
          __spark_metadata__: Spark.Dsl.Entity.spark_meta()
        }

  @doc false
  def __entity__,
    do: %Spark.Dsl.Entity{
      name: :middleware,
      describe: "Name a middleware to be added to the Reactor.",
      target: __MODULE__,
      args: [:module],
      identifier: :module,
      modules: [:module],
      schema: [
        module: [
          type: {:behaviour, Middleware},
          required: true,
          doc: """
          The middleware to be added to the Reactor.
          """
        ],
        description: [
          type: :string,
          required: false,
          doc: """
          An optional description for the middleware.
          """
        ]
      ]
    }

  defimpl Build do
    alias Reactor.Builder
    alias Spark.{Dsl.Extension, Error.DslError}

    def build(middleware, reactor) do
      Builder.add_middleware(reactor, middleware.module)
    end

    def verify(middleware, dsl_state) do
      cond do
        function_exported?(middleware.module, :get_process_context, 0) &&
            !function_exported?(middleware.module, :set_process_context, 1) ->
          {:error,
           %DslError{
             module: Extension.get_persisted(dsl_state, :module),
             path: [:reactor, :middlewares, :middleware],
             message: """
             When `get_process_context/0` is defined you must also defined `set_process_context/1`.
             """
           }}

        function_exported?(middleware.module, :set_process_context, 1) &&
            !function_exported?(middleware.module, :get_process_context, 0) ->
          {:error,
           %DslError{
             module: Extension.get_persisted(dsl_state, :module),
             path: [:reactor, :middlewares, :middleware],
             message: """
             When `set_process_context/1` is defined you must also defined `get_process_context/0`.
             """
           }}

        true ->
          :ok
      end
    end

    def transform(_, dsl_state), do: {:ok, dsl_state}
  end
end
