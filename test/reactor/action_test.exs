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
      action :celebrate, :string do
        argument :excitement_level, :integer, default: 3, allow_nil?: false

        run fn input, _context ->
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
    end
  end

  test "it runs the generic action" do
    assert {:ok, "Wooooooot"} = Reactor.run(SimpleActionReactor, %{excitement_level: 7})
  end
end
