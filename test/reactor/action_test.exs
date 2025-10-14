# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.ReactorActionTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule Wooter do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Simple, domain: Ash.Test.Domain

    attributes do
      uuid_primary_key :id
    end

    actions do
      default_accept :*

      action :celebrate, :string do
        argument :excitement_level, :integer, default: 3, allow_nil?: false

        run fn input, _context ->
          assert input.context.wat == 17

          level = Ash.ActionInput.get_argument(input, :excitement_level)
          ooo = Enum.map(1..level, fn _ -> "o" end)
          {:ok, Enum.join(["W"] ++ ooo ++ ["t"])}
        end
      end
    end
  end

  defmodule SimpleActionReactor do
    @moduledoc false
    use Reactor, extensions: [Ash.Reactor]

    ash do
      default_domain(Ash.Test.Domain)
    end

    input :excitement_level

    action :celebrate, Ash.Test.ReactorActionTest.Wooter, :celebrate do
      inputs(%{excitement_level: input(:excitement_level)})
      context(value(%{wat: 17}))
    end
  end

  test "it runs the generic action" do
    assert {:ok, "Wooooooot"} = Reactor.run(SimpleActionReactor, %{excitement_level: 7})
  end

  defmodule GuardedActionReactor do
    @moduledoc false
    use Reactor, extensions: [Ash.Reactor]

    ash do
      default_domain(Ash.Test.Domain)
    end

    input :excitement_level

    action :celebrate, Ash.Test.ReactorActionTest.Wooter, :celebrate do
      inputs(%{excitement_level: input(:excitement_level)})
      where &(&1.input.excitement_level > 3)
      context(value(%{wat: 17}))
    end
  end

  test "when the guard succeeds it runs the generic action" do
    assert {:ok, "Wooooooot"} = Reactor.run(GuardedActionReactor, %{excitement_level: 7})
  end

  test "when the guard fails it doesn't run the action" do
    assert {:ok, nil} = Reactor.run(GuardedActionReactor, %{excitement_level: 2})
  end
end
