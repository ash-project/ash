# SPDX-FileCopyrightText: 2022 spark contributors <https://github.com/ash-project/spark/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Spark.OptionsHelpers do
  @moduledoc """
  Helpers for working with options lists.
  """

  @deprecated "Use `Spark.Options.merge/3` instead"
  def merge_schemas(left, right, section \\ nil) do
    Spark.Options.merge(left, right, section)
  end

  @deprecated "Use `Spark.Options.validate/2` instead"
  def validate(opts, schema) do
    Spark.Options.validate(opts, schema)
  end

  @deprecated "Use `Spark.Options.validate!/2` instead"
  def validate!(opts, schema) do
    Spark.Options.validate!(opts, schema)
  end

  @doc """
  Creates markdown documentation for a given schema.
  """
  @deprecated "Use `Spark.Options.docs/1` instead"
  def docs(schema) do
    Spark.Options.docs(schema)
  end

  @deprecated "use `Spark.Options.Helpers.make_required!/2`"
  def make_required!(options, field) do
    Spark.Options.Helpers.make_required!(options, field)
  end

  @deprecated "use `Spark.Options.Helpers.make_optional!/2`"
  def make_optional!(options, field) do
    Spark.Options.Helpers.make_optional!(options, field)
  end

  @deprecated "use `Spark.Options.Helpers.set_type!/3`"
  def set_type!(options, field, type) do
    Spark.Options.Helpers.set_type!(options, field, type)
  end

  @deprecated "use `Spark.Options.Helpers.set_default!/3`"
  def set_default!(options, field, value) do
    Spark.Options.Helpers.set_default!(options, field, value)
  end

  @deprecated "use `Spark.Options.Helpers.append_doc!/3`"
  def append_doc!(options, field, to_append) do
    Spark.Options.Helpers.append_doc!(options, field, to_append)
  end
end
