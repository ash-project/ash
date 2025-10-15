# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Support.PolicyComplex.Comment.Checks.RuntimeCheck do
  @moduledoc false
  use Ash.Policy.Check

  def describe(_) do
    "sends a telemetry event when called"
  end

  def strict_check(_, _, _) do
    {:ok, :unknown}
  end

  def check(_, items, _, _) do
    send(self(), {:runtime_check_executed, items})

    if Process.get(:fail_runtime_check) do
      []
    else
      items
    end
  end
end
