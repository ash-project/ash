# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Resource.Validation.CompareTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Resource.Validation.Compare
  alias Ash.Test.Domain, as: Domain

  defmodule Post do
    use Ash.Resource, domain: Domain

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    attributes do
      uuid_primary_key :id

      attribute :number_one, :integer do
        public?(true)
      end

      attribute :number_two, :integer do
        public?(true)
      end

      attribute :number_three, :decimal do
        public?(true)
      end

      attribute :number_four, :float do
        public?(true)
      end

      attribute :list_value, {:array, :integer} do
        public?(true)
      end

      attribute :map_value, :map do
        public?(true)
      end

      attribute :tuple_value, :term do
        public?(true)
      end

      attribute :atom_value, :atom do
        public?(true)
      end

      attribute :string_value, :string do
        public?(true)
      end

      attribute :boolean_value, :boolean do
        public?(true)
      end

      attribute :nil_value, :term do
        public?(true)
      end
    end
  end

  describe "is_nil validation" do
    test "validates that a value is nil" do
      {:ok, opts} = Compare.init(attribute: :nil_value, is_nil: true)

      changeset = create_changeset(%{nil_value: nil})
      assert_validation_success(changeset, opts)

      changeset = create_changeset(%{nil_value: "not nil"})
      assert_validation_error(changeset, opts, "must be nil")
    end

    test "validates that a value is not nil" do
      {:ok, opts} = Compare.init(attribute: :nil_value, is_nil: false)

      changeset = create_changeset(%{nil_value: "not nil"})
      assert_validation_success(changeset, opts)

      changeset = create_changeset(%{nil_value: nil})
      assert_validation_error(changeset, opts, "must not be nil")
    end

    test "allows nil value when is_nil option is not specified" do
      {:ok, opts} = Compare.init(attribute: :nil_value)

      changeset = create_changeset(%{nil_value: nil})
      assert_validation_success(changeset, opts)
    end
  end

  describe "is_equal validation" do
    test "validates equality with numbers" do
      {:ok, opts} = Compare.init(attribute: :number_one, is_equal: 100)

      changeset = create_changeset(%{number_one: 100})
      assert_validation_success(changeset, opts)

      changeset = create_changeset(%{number_one: 1})
      assert_validation_error(changeset, opts, "must be equal to 100")
    end

    test "validates equality with arguments" do
      {:ok, opts} = Compare.init(attribute: :number_one, is_equal: :foo)

      changeset = create_changeset_with_arg(:foo, 100, %{number_one: 100})
      assert_validation_success(changeset, opts)

      changeset = create_changeset_with_arg(:foo, 100, %{number_one: 1})
      assert_validation_error(changeset, opts, "must be equal to foo")
    end

    test "validates equality with other attributes" do
      {:ok, opts} = Compare.init(attribute: :number_one, is_equal: :number_two)

      changeset = create_changeset(%{number_one: 100, number_two: 100})
      assert_validation_success(changeset, opts)

      changeset = create_changeset(%{number_one: 1, number_two: 100})
      assert_validation_error(changeset, opts, "must be equal to number_two")
    end

    test "validates equality with decimals" do
      {:ok, opts} = Compare.init(attribute: :number_three, is_equal: Decimal.new(1))
      changeset = create_changeset(%{number_three: Decimal.new(1)})
      assert_validation_success(changeset, opts)

      {:ok, opts} = Compare.init(attribute: :number_one, is_equal: Decimal.new(1))
      changeset = create_changeset(%{number_one: 1})
      assert_validation_success(changeset, opts)
    end

    test "validates equality with floats" do
      {:ok, opts} = Compare.init(attribute: :number_four, is_equal: 1.0)
      changeset = create_changeset(%{number_four: 1.0})
      assert_validation_success(changeset, opts)

      {:ok, opts} = Compare.init(attribute: :number_one, is_equal: 1.0)
      changeset = create_changeset(%{number_one: 1})
      assert_validation_success(changeset, opts)
    end

    test "validates equality with complex data types" do
      {:ok, opts} = Compare.init(attribute: :list_value, is_equal: [1, 2, 3])
      assert_validation_success(create_changeset(%{list_value: [1, 2, 3]}), opts)

      assert_validation_error(
        create_changeset(%{list_value: [4, 5, 6]}),
        opts,
        "must be equal to [1, 2, 3]"
      )

      {:ok, opts} = Compare.init(attribute: :map_value, is_equal: %{a: 1, b: 2})
      assert_validation_success(create_changeset(%{map_value: %{a: 1, b: 2}}), opts)

      assert_validation_error(
        create_changeset(%{map_value: %{a: 3, b: 4}}),
        opts,
        "must be equal to %{a: 1, b: 2}"
      )

      {:ok, opts} = Compare.init(attribute: :tuple_value, is_equal: {1, 2})
      assert_validation_success(create_changeset(%{tuple_value: {1, 2}}), opts)

      assert_validation_error(
        create_changeset(%{tuple_value: {3, 4}}),
        opts,
        "must be equal to {1, 2}"
      )
    end

    test "validates equality with special types" do
      {:ok, opts} = Compare.init(attribute: :atom_value, is_equal: {:value, :foo})
      assert_validation_success(create_changeset(%{atom_value: :foo}), opts)
      assert_validation_error(create_changeset(%{atom_value: :bar}), opts, "must be equal to foo")

      {:ok, opts} = Compare.init(attribute: :string_value, is_equal: "hello")
      assert_validation_success(create_changeset(%{string_value: "hello"}), opts)

      assert_validation_error(
        create_changeset(%{string_value: "world"}),
        opts,
        "must be equal to hello"
      )

      {:ok, opts} = Compare.init(attribute: :boolean_value, is_equal: {:value, true})
      assert_validation_success(create_changeset(%{boolean_value: true}), opts)

      assert_validation_error(
        create_changeset(%{boolean_value: false}),
        opts,
        "must be equal to true"
      )
    end
  end

  describe "is_not_equal validation" do
    test "validates inequality with numbers" do
      {:ok, opts} = Compare.init(attribute: :number_one, is_not_equal: 1)

      changeset = create_changeset(%{number_one: 100})
      assert_validation_success(changeset, opts)

      changeset = create_changeset(%{number_one: 1})
      assert_validation_error(changeset, opts, "must not be equal to 1")
    end

    test "validates inequality with arguments" do
      {:ok, opts} = Compare.init(attribute: :number_one, is_not_equal: :foo)

      changeset = create_changeset_with_arg(:foo, 1, %{number_one: 100})
      assert_validation_success(changeset, opts)

      changeset = create_changeset_with_arg(:foo, 100, %{number_one: 100})
      assert_validation_error(changeset, opts, "must not be equal to foo")
    end

    test "validates inequality with other attributes" do
      {:ok, opts} = Compare.init(attribute: :number_one, is_not_equal: :number_two)

      changeset = create_changeset(%{number_one: 100, number_two: 1})
      assert_validation_success(changeset, opts)

      changeset = create_changeset(%{number_one: 100, number_two: 100})
      assert_validation_error(changeset, opts, "must not be equal to number_two")
    end

    test "validates inequality with decimals" do
      {:ok, opts} = Compare.init(attribute: :number_three, is_not_equal: Decimal.new(1))
      changeset = create_changeset(%{number_three: Decimal.new(100)})
      assert_validation_success(changeset, opts)

      {:ok, opts} = Compare.init(attribute: :number_one, is_not_equal: Decimal.new(1))
      changeset = create_changeset(%{number_one: 1})
      assert_validation_error(changeset, opts, "must not be equal to 1")
    end

    test "validates inequality with complex data types" do
      {:ok, opts} = Compare.init(attribute: :list_value, is_not_equal: [1, 2, 3])
      assert_validation_success(create_changeset(%{list_value: [4, 5, 6]}), opts)

      assert_validation_error(
        create_changeset(%{list_value: [1, 2, 3]}),
        opts,
        "must not be equal to [1, 2, 3]"
      )

      {:ok, opts} = Compare.init(attribute: :map_value, is_not_equal: %{a: 1, b: 2})
      assert_validation_success(create_changeset(%{map_value: %{a: 3, b: 4}}), opts)

      assert_validation_error(
        create_changeset(%{map_value: %{a: 1, b: 2}}),
        opts,
        "must not be equal to %{a: 1, b: 2}"
      )

      {:ok, opts} = Compare.init(attribute: :tuple_value, is_not_equal: {1, 2})
      assert_validation_success(create_changeset(%{tuple_value: {3, 4}}), opts)

      assert_validation_error(
        create_changeset(%{tuple_value: {1, 2}}),
        opts,
        "must not be equal to {1, 2}"
      )
    end

    test "validates inequality with special types" do
      {:ok, opts} = Compare.init(attribute: :atom_value, is_not_equal: {:value, :foo})
      assert_validation_success(create_changeset(%{atom_value: :bar}), opts)

      assert_validation_error(
        create_changeset(%{atom_value: :foo}),
        opts,
        "must not be equal to foo"
      )

      {:ok, opts} = Compare.init(attribute: :string_value, is_not_equal: "hello")
      assert_validation_success(create_changeset(%{string_value: "world"}), opts)

      assert_validation_error(
        create_changeset(%{string_value: "hello"}),
        opts,
        "must not be equal to hello"
      )

      {:ok, opts} = Compare.init(attribute: :boolean_value, is_not_equal: {:value, true})
      assert_validation_success(create_changeset(%{boolean_value: false}), opts)

      assert_validation_error(
        create_changeset(%{boolean_value: true}),
        opts,
        "must not be equal to true"
      )
    end
  end

  describe "comparison validations" do
    test "validates greater_than" do
      {:ok, opts} = Compare.init(attribute: :number_one, greater_than: 1)

      assert_validation_success(create_changeset(%{number_one: 100}), opts)
      assert_validation_error(create_changeset(%{number_one: 1}), opts, "must be greater than 1")

      {:ok, opts} = Compare.init(attribute: :number_one, greater_than: :foo)
      assert_validation_success(create_changeset_with_arg(:foo, 1, %{number_one: 100}), opts)

      assert_validation_error(
        create_changeset_with_arg(:foo, 100, %{number_one: 1}),
        opts,
        "must be greater than foo"
      )

      {:ok, opts} = Compare.init(attribute: :number_one, greater_than: :number_two)
      assert_validation_success(create_changeset(%{number_one: 100, number_two: 1}), opts)

      assert_validation_error(
        create_changeset(%{number_one: 1, number_two: 100}),
        opts,
        "must be greater than number_two"
      )

      {:ok, opts} = Compare.init(attribute: :number_one, greater_than: {:value, 1})
      assert_validation_success(create_changeset(%{number_one: 100}), opts)
      assert_validation_error(create_changeset(%{number_one: 1}), opts, "must be greater than 1")
    end

    test "validates greater_than_or_equal_to with value syntax" do
      {:ok, opts} = Compare.init(attribute: :number_one, greater_than_or_equal_to: {:value, 10})
      assert_validation_success(create_changeset(%{number_one: 10}), opts)
      assert_validation_success(create_changeset(%{number_one: 100}), opts)

      assert_validation_error(
        create_changeset(%{number_one: 5}),
        opts,
        "must be greater than or equal to 10"
      )
    end

    test "validates less_than with value syntax" do
      {:ok, opts} = Compare.init(attribute: :number_one, less_than: {:value, 10})
      assert_validation_success(create_changeset(%{number_one: 5}), opts)
      assert_validation_error(create_changeset(%{number_one: 10}), opts, "must be less than 10")
    end

    test "validates less_than_or_equal_to with value syntax" do
      {:ok, opts} = Compare.init(attribute: :number_one, less_than_or_equal_to: {:value, 10})
      assert_validation_success(create_changeset(%{number_one: 5}), opts)
      assert_validation_success(create_changeset(%{number_one: 10}), opts)

      assert_validation_error(
        create_changeset(%{number_one: 15}),
        opts,
        "must be less than or equal to 10"
      )
    end

    test "validates with different numeric types" do
      {:ok, opts} = Compare.init(attribute: :number_three, greater_than: 0)
      assert_validation_success(create_changeset(%{number_three: Decimal.new(1)}), opts)

      {:ok, opts} = Compare.init(attribute: :number_one, greater_than: Decimal.new(0))
      assert_validation_success(create_changeset(%{number_one: 1}), opts)

      {:ok, opts} = Compare.init(attribute: :number_four, greater_than: 0)
      assert_validation_success(create_changeset(%{number_four: 1.0}), opts)

      {:ok, opts} = Compare.init(attribute: :number_one, greater_than: 0.0)
      assert_validation_success(create_changeset(%{number_one: 1}), opts)
    end
  end

  test "validates against a range of values" do
    {:ok, opts} =
      Compare.init(attribute: :number_one, greater_than: 0, less_than_or_equal_to: 10)

    changeset = create_changeset(%{number_one: -1})

    assert_validation_error(
      changeset,
      opts,
      "must be greater than 0 and must be less than or equal to 10"
    )

    changeset = create_changeset(%{number_one: 5})
    assert_validation_success(changeset, opts)

    changeset = create_changeset(%{number_one: 11})

    assert_validation_error(
      changeset,
      opts,
      "must be greater than 0 and must be less than or equal to 10"
    )
  end

  describe "error handling" do
    test "returns :ok when attribute cannot be fetched" do
      {:ok, opts} = Compare.init(attribute: :non_existent_attribute)

      changeset = create_changeset(%{})
      assert_validation_success(changeset, opts)
    end
  end

  describe "edge cases" do
    test "validates with nested data structures" do
      # Nested maps
      nested_map = %{outer: %{inner: 42}}
      {:ok, opts} = Compare.init(attribute: :map_value, is_equal: nested_map)
      assert_validation_success(create_changeset(%{map_value: nested_map}), opts)

      assert_validation_error(
        create_changeset(%{map_value: %{outer: %{inner: 43}}}),
        opts,
        "must be equal to %{outer: %{inner: 42}}"
      )

      # We can't test nested lists with the current attribute type
      # But we can test with tuples in a term field
      nested_structure = {1, {2, 3}}
      {:ok, opts} = Compare.init(attribute: :tuple_value, is_equal: nested_structure)
      assert_validation_success(create_changeset(%{tuple_value: nested_structure}), opts)

      assert_validation_error(
        create_changeset(%{tuple_value: {1, {2, 4}}}),
        opts,
        "must be equal to {1, {2, 3}}"
      )
    end

    test "validates with empty values" do
      # Empty string
      {:ok, opts} = Compare.init(attribute: :string_value, is_equal: "")
      assert_validation_success(create_changeset(%{string_value: ""}), opts)

      assert_validation_error(
        create_changeset(%{string_value: "not empty"}),
        opts,
        "must be equal to "
      )

      # Empty list
      {:ok, opts} = Compare.init(attribute: :list_value, is_equal: [])
      assert_validation_success(create_changeset(%{list_value: []}), opts)

      assert_validation_error(
        create_changeset(%{list_value: [1, 2, 3]}),
        opts,
        "must be equal to []"
      )

      # Empty map
      {:ok, opts} = Compare.init(attribute: :map_value, is_equal: %{})
      assert_validation_success(create_changeset(%{map_value: %{}}), opts)

      assert_validation_error(
        create_changeset(%{map_value: %{a: 1}}),
        opts,
        "must be equal to %{}"
      )
    end

    test "validates with function values" do
      fun = fn -> 42 end
      {:ok, opts} = Compare.init(attribute: :number_one, greater_than: fun)

      assert_validation_success(create_changeset(%{number_one: 100}), opts)

      assert_validation_error(
        create_changeset(%{number_one: 10}),
        opts,
        "must be greater than 42"
      )
    end
  end

  defp create_changeset(attrs) do
    Post |> Ash.Changeset.for_create(:create, attrs)
  end

  defp create_changeset_with_arg(arg_name, arg_value, attrs) do
    Post
    |> Ash.Changeset.new()
    |> Ash.Changeset.set_argument(arg_name, arg_value)
    |> Ash.Changeset.for_create(:create, attrs)
  end

  defp assert_validation_success(changeset, opts) do
    assert :ok = Compare.validate(changeset, opts, %{})
  end

  defp assert_validation_error(changeset, opts, expected_message) do
    assert {:error, %{message: message, vars: vars}} = Compare.validate(changeset, opts, %{})
    assert expected_message == translate_message(message, vars)
  end

  defp translate_message(message, vars) do
    Enum.reduce(vars, message, fn {key, value}, acc ->
      if String.contains?(acc, "%{#{key}}") do
        String.replace(acc, "%{#{key}}", to_string(value))
      else
        acc
      end
    end)
  end
end
