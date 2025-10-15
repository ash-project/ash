# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.ReactorAshStepTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule GenericAshStep do
    @moduledoc false
    use Ash.Reactor
    input(:title)

    ash_step :generic_ash_step do
      argument :title, input(:title)

      run fn input, _context ->
        {:ok, "hello #{input.title}"}
      end
    end
  end

  test "it runs the step" do
    assert {:ok, "hello world"} = Reactor.run(GenericAshStep, %{title: "world"})
  end
end
