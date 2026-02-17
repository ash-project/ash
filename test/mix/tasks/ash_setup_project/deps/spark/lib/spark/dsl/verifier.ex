# SPDX-FileCopyrightText: 2022 spark contributors <https://github.com/ash-project/spark/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Spark.Dsl.Verifier do
  @moduledoc """
  A verifier gets the dsl state and can return `:ok` or `:error`.

  In a verifier, you can reference and depend on other modules without causing compile time dependencies.
  """

  @type warning() :: String.t() | {String.t(), :erl_anno.anno()}

  @callback verify(map) ::
              :ok
              | {:error, term}
              | {:warn, warning() | list(warning())}

  defmacro __using__(_) do
    quote generated: true do
      @behaviour Spark.Dsl.Verifier
    end
  end

  defdelegate get_persisted(dsl, key), to: Spark.Dsl.Transformer
  defdelegate get_persisted(dsl, key, default), to: Spark.Dsl.Transformer
  defdelegate get_option(dsl_state, path, option), to: Spark.Dsl.Transformer
  defdelegate get_option(dsl_state, path, option, default), to: Spark.Dsl.Transformer
  defdelegate fetch_option(dsl_state, path, option), to: Spark.Dsl.Transformer
  defdelegate get_entities(dsl_state, path), to: Spark.Dsl.Transformer
end
