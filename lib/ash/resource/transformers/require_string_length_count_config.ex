# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Transformers.RequireStringLengthCountConfig do
  @moduledoc """
  Requires `config :ash, :default_string_length_count` to be set.
  """
  use Spark.Dsl.Transformer

  alias Spark.Dsl.Transformer
  alias Spark.Error.DslError

  def transform(dsl_state) do
    if has_string_attribute?(dsl_state) do
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
    else
      {:ok, dsl_state}
    end
  end

  defp has_string_attribute?(dsl_state) do
    dsl_state
    |> Transformer.get_entities([:attributes])
    |> Enum.any?(&string_type?(&1.type))
  end

  defp string_type?({:array, type}), do: string_type?(type)
  defp string_type?(type), do: type in [Ash.Type.String, Ash.Type.CiString]
end
