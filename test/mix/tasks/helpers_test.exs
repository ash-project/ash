defmodule Ash.Mix.Tasks.HelpersTest do
  use ExUnit.Case, async: true

  alias Ash.Mix.Tasks.Helpers

  defmodule ExtA do
  end

  defmodule ExtB do
  end

  defmodule ExtC do
  end

  defmodule ExtDependsOnA do
    def dependencies, do: [ExtA]
  end

  defmodule ExtDependsOnAAndB do
    def dependencies, do: [ExtA, ExtB]
  end

  defmodule ExtCycleB do
    def dependencies, do: [Ash.Mix.Tasks.HelpersTest.ExtCycleA]
  end

  defmodule ExtCycleA do
    def dependencies, do: [ExtCycleB]
  end

  describe "sort_extensions/1" do
    test "returns extensions unchanged when none declare dependencies" do
      result = Helpers.sort_extensions([ExtA, ExtB, ExtC])
      assert MapSet.new(result) == MapSet.new([ExtA, ExtB, ExtC])
    end

    test "places dependencies before dependents" do
      result = Helpers.sort_extensions([ExtDependsOnA, ExtA])
      assert Enum.find_index(result, &(&1 == ExtA)) < Enum.find_index(result, &(&1 == ExtDependsOnA))
    end

    test "handles multiple dependencies" do
      result = Helpers.sort_extensions([ExtDependsOnAAndB, ExtA, ExtB])

      a_idx = Enum.find_index(result, &(&1 == ExtA))
      b_idx = Enum.find_index(result, &(&1 == ExtB))
      dep_idx = Enum.find_index(result, &(&1 == ExtDependsOnAAndB))

      assert a_idx < dep_idx
      assert b_idx < dep_idx
    end

    test "ignores dependencies not present in the list" do
      # ExtDependsOnA depends on ExtA, but ExtA is not in the list
      result = Helpers.sort_extensions([ExtDependsOnA, ExtB])
      assert MapSet.new(result) == MapSet.new([ExtDependsOnA, ExtB])
    end

    test "handles empty list" do
      assert Helpers.sort_extensions([]) == []
    end

    test "handles single extension" do
      assert Helpers.sort_extensions([ExtA]) == [ExtA]
    end

    test "raises on circular dependencies" do
      assert_raise RuntimeError, ~r/Circular dependency detected/, fn ->
        Helpers.sort_extensions([ExtCycleA, ExtCycleB])
      end
    end

    test "handles transitive dependencies" do
      # ExtDependsOnA -> ExtA, ExtDependsOnAAndB -> ExtA, ExtB
      result = Helpers.sort_extensions([ExtDependsOnAAndB, ExtDependsOnA, ExtA, ExtB])

      a_idx = Enum.find_index(result, &(&1 == ExtA))
      depends_on_a_idx = Enum.find_index(result, &(&1 == ExtDependsOnA))
      depends_on_ab_idx = Enum.find_index(result, &(&1 == ExtDependsOnAAndB))

      assert a_idx < depends_on_a_idx
      assert a_idx < depends_on_ab_idx
    end
  end
end
