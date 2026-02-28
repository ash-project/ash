defmodule Ash.Test.Type.TypeResolverTest do
  @moduledoc false
  use ExUnit.Case, async: false

  alias Ash.TypeResolver

  setup do
    # Clean up any existing TypeResolver
    TypeResolver.shutdown()
    :ok
  end

  describe "TypeResolver GenServer" do
    test "ensure_started is idempotent" do
      assert :ok = TypeResolver.ensure_started()
      assert :ok = TypeResolver.ensure_started()
      TypeResolver.shutdown()
    end

    test "resolves immediately when type is already known" do
      TypeResolver.ensure_started()

      TypeResolver.register_known_field(MyResource, :title, Ash.Type.String, [])

      assert {:ok, Ash.Type.String, []} = TypeResolver.resolve(MyResource, :title)

      TypeResolver.shutdown()
    end

    test "deadlock detection finds cycles" do
      TypeResolver.ensure_started()

      # Create a circular dependency: A depends on B, B depends on A
      fake_calc_a = %Ash.Resource.Calculation{
        name: :calc_a,
        type: :auto,
        calculation: {Ash.Resource.Calculation.Expression, expr: nil}
      }

      fake_calc_b = %Ash.Resource.Calculation{
        name: :calc_b,
        type: :auto,
        calculation: {Ash.Resource.Calculation.Expression, expr: nil}
      }

      # Register A depending on B
      TypeResolver.register_auto(
        ResourceA,
        :calc_a,
        fake_calc_a,
        %{},
        [{ResourceB, :calc_b}]
      )

      # Register B depending on A
      TypeResolver.register_auto(
        ResourceB,
        :calc_b,
        fake_calc_b,
        %{},
        [{ResourceA, :calc_a}]
      )

      TypeResolver.done_registering(ResourceA)
      TypeResolver.done_registering(ResourceB)

      # Trying to resolve should detect the cycle
      result = TypeResolver.resolve(ResourceA, :calc_a, 5_000)
      assert {:error, message} = result
      assert message =~ "Circular dependency"

      TypeResolver.shutdown()
    end
  end
end
