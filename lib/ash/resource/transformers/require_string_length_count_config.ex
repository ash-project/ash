# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Transformers.RequireStringLengthCountConfig do
  @moduledoc """
  Requires `config :ash, :default_string_length_count` to be set.

  Every application must make an explicit choice about how string length is
  counted. See the backwards compatibility guide for details.

  The check only runs for resources that belong to the current OTP application,
  i.e. the project being compiled, not its dependencies. Resources shipped by
  libraries are compiled before the host application's installer has run, and
  the library has no say over the host application's configuration. Those
  resources are still covered by the runtime check in
  `Ash.Type.String.length_count_config/0`.
  """
  use Spark.Dsl.Transformer

  alias Spark.Dsl.Transformer
  alias Spark.Error.DslError

  def transform(dsl_state) do
    if compiling_dependency?() do
      {:ok, dsl_state}
    else
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

  # `Mix.Dep.in_dependency/3` sets `:deps_app_path` in the project config while
  # a dependency is being compiled. The root project never has it set.
  defp compiling_dependency? do
    Code.ensure_loaded?(Mix.Project) and function_exported?(Mix.Project, :config, 0) and
      Keyword.has_key?(Mix.Project.config(), :deps_app_path)
  end
end
