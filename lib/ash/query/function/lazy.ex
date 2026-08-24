# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Function.Lazy do
  @moduledoc """
  Runs the provided MFA and returns the result as a known value.

  Evaluated just before running the query.
  """
  use Ash.Query.Function, name: :lazy, eager_evaluate?: false

  def args, do: [[:any]]
  def returns, do: [:any]

  # `lazy` runs an arbitrary MFA, so it must never be reachable from
  # untrusted input (`Ash.Filter.parse_input/2`, `Ash.Query.filter_input/2`).
  def private?, do: true

  def evaluate(%{arguments: [{m, f, a}]}), do: {:known, apply(m, f, a)}
end
