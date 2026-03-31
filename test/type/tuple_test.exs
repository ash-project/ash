# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Type.TupleTest do
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain
  alias Ash.Test.DumpTestType

  defmodule TupleWithFields do
    use Ash.Type.NewType,
      subtype_of: :tuple,
      constraints: [
        fields: [
          foo: [type: :string, allow_nil?: false]
        ]
      ]
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    attributes do
      uuid_primary_key :id

      attribute :metadata_type, TupleWithFields do
        public? true
      end

      attribute :metadata, :tuple do
        public? true

        constraints fields: [
                      foo: [type: :string, allow_nil?: false],
                      integer_min_0: [type: :integer, constraints: [min: 0]]
                    ]
      end
    end
  end

  test "it handles valid tuples" do
    changeset =
      Post
      |> Ash.Changeset.for_create(:create, %{
        metadata: {"bar", 1},
        metadata_type: {"bar"}
      })

    assert changeset.valid?
  end

  test "allow_nil? is true by default" do
    changeset =
      Post
      |> Ash.Changeset.for_create(:create, %{
        metadata: {"bar", "2"}
      })

    assert changeset.valid?

    assert changeset.attributes == %{
             metadata: {"bar", 2}
           }
  end

  test "keys that can be nil don't need to be there" do
    changeset =
      Post
      |> Ash.Changeset.for_create(:create, %{
        metadata: {
          "bar",
          nil
        }
      })

    assert changeset.valid?
  end

  test "keys that can not be nil need to be there" do
    changeset =
      Post
      |> Ash.Changeset.for_create(:create, %{
        metadata: {nil, 1}
      })

    refute changeset.valid?

    assert [
             %Ash.Error.Changes.InvalidAttribute{
               field: :foo,
               message: "value must not be nil",
               private_vars: nil,
               value: {nil, 1},
               bread_crumbs: [],
               vars: [],
               path: [:metadata]
             }
           ] = changeset.errors
  end

  test "constraints of field types are checked" do
    changeset =
      Post
      |> Ash.Changeset.for_create(:create, %{
        metadata: {"hello", -1}
      })

    refute changeset.valid?

    assert [
             %Ash.Error.Changes.InvalidAttribute{
               field: :integer_min_0,
               message: "must be greater than or equal to %{min}",
               private_vars: nil,
               value: {"hello", -1},
               bread_crumbs: [],
               vars: [min: 0],
               path: [:metadata]
             }
           ] = changeset.errors
  end

  test "apply_constraints handles nil values" do
    assert {:ok, nil} = Ash.Type.Tuple.apply_constraints(nil, fields: [foo: [type: :string]])
  end

  test "values are casted before checked" do
    changeset =
      Post
      |> Ash.Changeset.for_create(
        :create,
        %{
          metadata: {"", "2"}
        }
      )

    refute changeset.valid?

    assert [
             %Ash.Error.Changes.InvalidAttribute{
               field: :foo,
               message: "value must not be nil",
               private_vars: nil,
               value: {"", "2"},
               bread_crumbs: [],
               vars: [],
               path: [:metadata]
             }
           ] = changeset.errors
  end

  describe "dump_to_native" do
    test "recursively dumps field types" do
      {:ok, constraints} =
        Ash.Type.init(Ash.Type.Tuple,
          fields: [
            name: [type: DumpTestType],
            count: [type: :integer]
          ]
        )

      assert {:ok, %{name: "native:hello", count: 42}} =
               Ash.Type.dump_to_native(Ash.Type.Tuple, {"hello", 42}, constraints)
    end

    test "handles nil" do
      assert {:ok, nil} =
               Ash.Type.dump_to_native(Ash.Type.Tuple, nil, fields: [foo: [type: :string]])
    end

    test "returns error for wrong tuple size" do
      {:ok, constraints} =
        Ash.Type.init(Ash.Type.Tuple,
          fields: [
            a: [type: :string],
            b: [type: :string]
          ]
        )

      assert :error = Ash.Type.dump_to_native(Ash.Type.Tuple, {"only_one"}, constraints)
    end

    test "returns error for non-tuple values" do
      {:ok, constraints} =
        Ash.Type.init(Ash.Type.Tuple, fields: [a: [type: :string]])

      assert :error = Ash.Type.dump_to_native(Ash.Type.Tuple, "string", constraints)
      assert :error = Ash.Type.dump_to_native(Ash.Type.Tuple, %{a: "map"}, constraints)
    end

    test "handles nil field values" do
      {:ok, constraints} =
        Ash.Type.init(Ash.Type.Tuple,
          fields: [
            name: [type: :string],
            age: [type: :integer]
          ]
        )

      assert {:ok, %{name: nil, age: nil}} =
               Ash.Type.dump_to_native(Ash.Type.Tuple, {nil, nil}, constraints)
    end
  end

  describe "dump_to_embedded" do
    test "recursively calls dump_to_embedded on field types" do
      {:ok, constraints} =
        Ash.Type.init(Ash.Type.Tuple,
          fields: [
            name: [type: DumpTestType],
            count: [type: :integer]
          ]
        )

      assert {:ok, %{name: "embedded:hello", count: 42}} =
               Ash.Type.dump_to_embedded(Ash.Type.Tuple, {"hello", 42}, constraints)
    end

    test "uses dump_to_embedded not dump_to_native on fields" do
      {:ok, constraints} =
        Ash.Type.init(Ash.Type.Tuple,
          fields: [
            val: [type: DumpTestType]
          ]
        )

      {:ok, native_result} =
        Ash.Type.dump_to_native(Ash.Type.Tuple, {"test"}, constraints)

      {:ok, embedded_result} =
        Ash.Type.dump_to_embedded(Ash.Type.Tuple, {"test"}, constraints)

      assert native_result[:val] == "native:test"
      assert embedded_result[:val] == "embedded:test"
    end

    test "handles nil" do
      assert {:ok, nil} =
               Ash.Type.dump_to_embedded(Ash.Type.Tuple, nil, fields: [foo: [type: :string]])
    end

    test "returns error for non-tuple values" do
      {:ok, constraints} =
        Ash.Type.init(Ash.Type.Tuple, fields: [a: [type: :string]])

      assert :error = Ash.Type.dump_to_embedded(Ash.Type.Tuple, "string", constraints)
    end

    test "returns error for wrong tuple size" do
      {:ok, constraints} =
        Ash.Type.init(Ash.Type.Tuple,
          fields: [
            a: [type: :string],
            b: [type: :string]
          ]
        )

      assert :error = Ash.Type.dump_to_embedded(Ash.Type.Tuple, {"only_one"}, constraints)
    end
  end

  describe "dump/cast round-trip" do
    test "dump_to_native then cast_stored preserves data" do
      {:ok, constraints} =
        Ash.Type.init(Ash.Type.Tuple,
          fields: [
            name: [type: :string],
            count: [type: :integer]
          ]
        )

      original = {"hello", 42}

      assert {:ok, dumped} = Ash.Type.dump_to_native(Ash.Type.Tuple, original, constraints)
      assert {:ok, restored} = Ash.Type.cast_stored(Ash.Type.Tuple, dumped, constraints)
      assert restored == original
    end

    test "dump_to_native then cast_stored with string keys round-trips" do
      {:ok, constraints} =
        Ash.Type.init(Ash.Type.Tuple,
          fields: [
            name: [type: :string],
            count: [type: :integer]
          ]
        )

      original = {"hello", 42}

      assert {:ok, dumped} = Ash.Type.dump_to_native(Ash.Type.Tuple, original, constraints)

      # Simulate stored data having string keys
      string_keyed = Map.new(dumped, fn {k, v} -> {to_string(k), v} end)

      assert {:ok, restored} = Ash.Type.cast_stored(Ash.Type.Tuple, string_keyed, constraints)
      assert restored == original
    end
  end
end
