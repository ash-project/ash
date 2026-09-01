# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Actions.Read.AsyncLimiterTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Actions.Read.AsyncLimiter

  test "await_at_least_one blocks for pending tasks instead of busy-spinning" do
    fast = Task.async(fn -> Process.sleep(50) end)
    slow = Task.async(fn -> Process.sleep(400) end)

    {:reductions, before} = Process.info(self(), :reductions)
    {complete, remaining} = AsyncLimiter.await_at_least_one([fast, slow])
    {:reductions, aft} = Process.info(self(), :reductions)

    assert length(complete) == 1
    assert [%Task{}] = remaining

    assert aft - before < 500_000

    Task.shutdown(slow)
  end
end
