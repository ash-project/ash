# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Transformers.RequireStringLengthCountConfig do
  @moduledoc """
  Requires `config :ash, :default_string_length_count` to be set.

  Every application must make an explicit choice about how string length is
  counted. See the backwards compatibility guide for details.
  """
  use Spark.Dsl.Transformer

  alias Spark.Dsl.Transformer
  alias Spark.Error.DslError

  def transform(dsl_state) do
    value = Application.get_env(:ash, :default_string_length_count)

    if value in [:codepoints, :mixed] do
      {:ok, dsl_state}
    else
      {:error,
       DslError.exception(
         module: Transformer.get_persisted(dsl_state, :module),
         message: Ash.Type.String.length_count_config_error(value),
         path: []
       )}
    end
  end
end
