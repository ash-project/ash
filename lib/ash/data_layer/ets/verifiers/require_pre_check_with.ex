# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.DataLayer.Ets.Verifiers.RequirePreCheckWith do
  @moduledoc """
  Ensures that every identity of a resource outside a `transaction_group` has a
  `pre_check_with` configured, or raises. A resource in a transaction group has its
  identities enforced when it writes.
  """
  use Spark.Dsl.Verifier

  @impl true
  def verify(dsl) do
    if Ash.DataLayer.Ets.Info.transaction_group(dsl) do
      :ok
    else
      Ash.DataLayer.Verifiers.RequirePreCheckWith.verify(dsl)
    end
  end
end
