# SPDX-FileCopyrightText: 2022 spark contributors <https://github.com/ash-project/spark/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Spark.Dsl.Extension.SectionOption do
  @moduledoc false

  def value_and_function(value, field, type, caller, section_modules, section_no_depend_modules) do
    value =
      case type do
        :quoted ->
          Macro.escape(value)

        _ ->
          value
      end

    value =
      cond do
        field in section_modules ->
          Spark.Dsl.Extension.expand_alias(value, caller)

        field in section_no_depend_modules ->
          Spark.Dsl.Extension.expand_alias_no_require(value, caller)

        true ->
          value
      end

    Spark.CodeHelpers.lift_functions(value, field, caller)
  end

  def set_section_option(module, extension, section_path, field, value, anno \\ nil) do
    current_sections = Process.get({module, :spark_sections}, [])

    unless {extension, section_path} in current_sections do
      Process.put({module, :spark_sections}, [
        {extension, section_path} | current_sections
      ])
    end

    current_config =
      Process.get(
        {module, :spark, section_path},
        Spark.Dsl.Extension.default_section_config()
      )

    opts_anno =
      if anno do
        Keyword.put(current_config[:opts_anno] || [], field, anno)
      else
        current_config[:opts_anno] || []
      end

    Process.put(
      {module, :spark, section_path},
      %{
        current_config
        | opts: Keyword.put(current_config.opts, field, value),
          opts_anno: opts_anno
      }
    )
  end
end
