# SPDX-FileCopyrightText: 2022 spark contributors <https://github.com/ash-project/spark/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Spark.Mix.Helpers do
  @moduledoc false

  def extension_name(extension, opts) do
    extension
    |> inspect()
    |> strip_prefix(opts)
    |> String.trim_trailing(".Dsl")
    |> String.split(".")
    |> Enum.join(".")
  end

  defp strip_prefix(extension, opts) do
    if opts[:strip_prefix] do
      String.trim_leading(extension, opts[:strip_prefix])
    else
      extension
    end
  end
end
