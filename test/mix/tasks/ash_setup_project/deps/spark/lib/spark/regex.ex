# SPDX-FileCopyrightText: 2022 spark contributors <https://github.com/ash-project/spark/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Spark.Regex do
  @moduledoc """
  Utilities for caching compiled regular expressions.

  This module provides a way to cache compiled regular expressions in persistent_term
  to work around OTP 28's restriction on compile-time regex creation.
  """

  @doc """
  Retrieves a cached regex or compiles and caches it if not found.

  ## Parameters

    * `source` - The regex source string
    * `opts` - The regex compilation options (e.g., "ims", "u", etc.)

  ## Returns

  The compiled `Regex` struct.

  ## Examples

      iex> Spark.Regex.cache("foo.*bar", "i")
      ~r/foo.*bar/i

  """
  def cache(source, opts) do
    key = {__MODULE__, source, opts}

    case :persistent_term.get(key, :not_found) do
      :not_found ->
        regex = Regex.compile!(source, opts)
        :persistent_term.put(key, regex)
        regex

      regex ->
        regex
    end
  end
end
