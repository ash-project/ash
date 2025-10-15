# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Resource.Changes.UpdateChangeTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule TimeMachine do
    @moduledoc false
    def double_age(42) do
      raise "boom"
    end

    def double_age(age) when is_integer(age) and age >= 0 and age < 200 do
      age * 2
    end
  end

  defmodule Author do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets, domain: Ash.Test.Domain

    ets do
      private?(true)
    end

    actions do
      create :double_age do
        accept([:name, :age])
        change update_change(:age, &TimeMachine.double_age/1)
        validate numericality(:age, less_than: 200)
      end

      create :triple_age do
        accept([:name, :age])
        change update_change(:age, fn age -> age * 3 end)
      end
    end

    attributes do
      uuid_primary_key :id
      attribute(:name, :string, public?: true, allow_nil?: false)
      attribute(:age, :integer, public?: true, allow_nil?: false, constraints: [min: 0])
    end
  end

  describe "update_change builtin" do
    test "works with valid change" do
      author =
        Author
        |> Ash.Changeset.for_create(:double_age, %{name: "foo", age: 20})
        |> Ash.create!()

      assert author.age == 40
    end

    test "works with inline function" do
      author =
        Author
        |> Ash.Changeset.for_create(:triple_age, %{name: "foo", age: 20})
        |> Ash.create!()

      assert author.age == 60
    end

    test "calls update function with casted attribute" do
      author =
        Author
        |> Ash.Changeset.for_create(:double_age, %{name: "foo", age: "20"})
        |> Ash.create!()

      assert author.age == 40
    end

    test "doesn't call update function with failed constraint" do
      {:error, _} =
        Author
        |> Ash.Changeset.for_create(:double_age, %{name: "foo", age: -5})
        |> Ash.create()
    end

    test "doesn't call update function with missing required attribute" do
      {:error, _} =
        Author
        |> Ash.Changeset.for_create(:double_age, %{name: "foo"})
        |> Ash.create()
    end

    test "doesn't call update function with other changeset errors" do
      {:error, _} =
        Author
        |> Ash.Changeset.for_create(:double_age, %{age: 42})
        |> Ash.create()
    end
  end
end
