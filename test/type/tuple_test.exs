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

  describe "casting a map keyed by field name" do
    test "casts into a tuple in field order" do
      changeset =
        Post
        |> Ash.Changeset.for_create(:create, %{
          metadata: %{"integer_min_0" => "2", "foo" => "bar"},
          metadata_type: %{foo: "baz"}
        })

      assert changeset.valid?

      assert changeset.attributes == %{
               metadata: {"bar", 2},
               metadata_type: {"baz"}
             }
    end

    test "a missing field takes nil in its position" do
      changeset =
        Post
        |> Ash.Changeset.for_create(:create, %{metadata: %{"foo" => "bar"}})

      assert changeset.valid?
      assert changeset.attributes == %{metadata: {"bar", nil}}
    end

    test "a missing field that does not allow nil is rejected" do
      changeset =
        Post
        |> Ash.Changeset.for_create(:create, %{metadata: %{"integer_min_0" => 1}})

      refute changeset.valid?

      assert [
               %Ash.Error.Changes.InvalidAttribute{
                 field: :foo,
                 message: "value must not be nil",
                 path: [:metadata]
               }
             ] = changeset.errors
    end

    test "undeclared keys are ignored" do
      assert {:ok, {"bar", nil}} =
               Ash.Type.cast_input(
                 Ash.Type.Tuple,
                 %{"foo" => "bar", "other" => 1},
                 fields: [foo: [type: :string], integer_min_0: [type: :integer]]
               )
    end

    test "structs are not cast" do
      assert {:error, _} =
               Ash.Type.cast_input(Ash.Type.Tuple, ~D[2024-01-01], fields: [foo: [type: :string]])
    end
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

      assert {:ok, %{name: "embedded:hello", count: 42}} =
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

  describe "JSON safety of dump_to_native (#2747)" do
    test "dumped tuple with binary-native field types stays JSON-encodable" do
      {:ok, constraints} =
        Ash.Type.init(Ash.Type.Tuple,
          fields: [
            id: [type: :uuid_v7],
            data: [type: :binary]
          ]
        )

      uuid = Ash.UUIDv7.generate()
      binary = <<1, 158, 179, 13, 254, 207>>

      {:ok, dumped} =
        Ash.Type.dump_to_native(Ash.Type.Tuple, {uuid, binary}, constraints)

      json = Jason.encode!(dumped)

      {:ok, loaded} =
        Ash.Type.cast_stored(Ash.Type.Tuple, Jason.decode!(json), constraints)

      assert loaded == {uuid, binary}
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

    test "dump_to_native also dumps fields with dump_to_embedded, for JSON safety" do
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

      assert native_result[:val] == "embedded:test"
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

  describe "cast_from_embedded" do
    test "recursively calls cast_from_embedded on field types" do
      {:ok, constraints} =
        Ash.Type.init(Ash.Type.Tuple,
          fields: [
            name: [type: DumpTestType],
            count: [type: :integer]
          ]
        )

      assert {:ok, {"hello", 42}} =
               Ash.Type.cast_from_embedded(
                 Ash.Type.Tuple,
                 %{name: "embedded:hello", count: 42},
                 constraints
               )
    end

    test "handles nil" do
      assert {:ok, nil} =
               Ash.Type.cast_from_embedded(Ash.Type.Tuple, nil, fields: [foo: [type: :string]])
    end

    test "returns error for non-map values" do
      {:ok, constraints} =
        Ash.Type.init(Ash.Type.Tuple, fields: [a: [type: :string]])

      assert :error = Ash.Type.cast_from_embedded(Ash.Type.Tuple, "string", constraints)
    end

    test "handles string keys" do
      {:ok, constraints} =
        Ash.Type.init(Ash.Type.Tuple,
          fields: [
            name: [type: DumpTestType],
            count: [type: :integer]
          ]
        )

      assert {:ok, {"hello", 42}} =
               Ash.Type.cast_from_embedded(
                 Ash.Type.Tuple,
                 %{"name" => "embedded:hello", "count" => 42},
                 constraints
               )
    end
  end

  describe "loading invalid stored values" do
    setup do
      {:ok, constraints} =
        Ash.Type.init(Ash.Type.Tuple,
          fields: [
            a: [type: :string, allow_nil?: false],
            b: [type: :integer],
            c: [type: :string]
          ]
        )

      %{constraints: constraints}
    end

    test "cast_stored returns an error when a field fails to cast", %{constraints: constraints} do
      assert :error =
               Ash.Type.cast_stored(
                 Ash.Type.Tuple,
                 %{"a" => "x", "b" => "abc", "c" => "z"},
                 constraints
               )
    end

    test "cast_from_embedded returns an error when a field fails to cast", %{
      constraints: constraints
    } do
      assert :error =
               Ash.Type.cast_from_embedded(
                 Ash.Type.Tuple,
                 %{"a" => "x", "b" => "abc", "c" => "z"},
                 constraints
               )
    end

    test "a missing first field loads as nil in its position", %{constraints: constraints} do
      assert {:ok, {nil, 1, "z"}} =
               Ash.Type.cast_stored(Ash.Type.Tuple, %{"b" => 1, "c" => "z"}, constraints)
    end

    test "a missing middle field loads as nil in its position", %{constraints: constraints} do
      assert {:ok, {"x", nil, "z"}} =
               Ash.Type.cast_stored(Ash.Type.Tuple, %{"a" => "x", "c" => "z"}, constraints)

      assert {:ok, {"x", nil, "z"}} =
               Ash.Type.cast_from_embedded(Ash.Type.Tuple, %{a: "x", c: "z"}, constraints)
    end

    test "a missing last field loads as nil in its position", %{constraints: constraints} do
      assert {:ok, {"x", 1, nil}} =
               Ash.Type.cast_stored(Ash.Type.Tuple, %{"a" => "x", "b" => 1}, constraints)
    end

    test "apply_constraints rejects a missing field that does not allow nil", %{
      constraints: constraints
    } do
      assert {:ok, value} =
               Ash.Type.cast_stored(Ash.Type.Tuple, %{"b" => 1, "c" => "z"}, constraints)

      assert {:error, [[message: "value must not be nil", field: :a]]} =
               Ash.Type.apply_constraints(Ash.Type.Tuple, value, constraints)
    end
  end

  test "init rejects duplicate field names" do
    assert {:error, "fields must have unique names, got duplicates `[:a]`"} =
             Ash.Type.Tuple.init(fields: [a: [type: :integer], a: [type: :string]])
  end

  describe "load" do
    test "returns an error for load keys that are not declared fields" do
      {:ok, constraints} = Ash.Type.init(Ash.Type.Tuple, fields: [foo: [type: :string]])

      assert {:error, "cannot load [:unknown]" <> _} =
               Ash.Type.Tuple.load([{"bar"}], [:unknown], constraints, %{})
    end
  end

  describe "apply_constraints" do
    test "rejects tuples with the wrong number of elements" do
      {:ok, constraints} =
        Ash.Type.init(Ash.Type.Tuple, fields: [a: [type: :string], b: [type: :string]])

      assert {:error, [message: _, expected_length: 2, value_length: 1]} =
               Ash.Type.apply_constraints(Ash.Type.Tuple, {"x"}, constraints)
    end

    test "reports every invalid field" do
      {:ok, constraints} =
        Ash.Type.init(Ash.Type.Tuple, fields: [a: [type: :integer], b: [type: :integer]])

      assert {:error,
              [
                [message: "is invalid", field: :a, value: "x"],
                [message: "is invalid", field: :b, value: "y"]
              ]} =
               Ash.Type.apply_constraints(Ash.Type.Tuple, {"x", "y"}, constraints)
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

    test "dump_to_embedded then cast_from_embedded preserves data" do
      {:ok, constraints} =
        Ash.Type.init(Ash.Type.Tuple,
          fields: [
            name: [type: :string],
            count: [type: :integer]
          ]
        )

      original = {"hello", 42}

      assert {:ok, dumped} = Ash.Type.dump_to_embedded(Ash.Type.Tuple, original, constraints)
      assert {:ok, restored} = Ash.Type.cast_from_embedded(Ash.Type.Tuple, dumped, constraints)
      assert restored == original
    end

    test "dump_to_embedded then cast_from_embedded with string keys round-trips" do
      {:ok, constraints} =
        Ash.Type.init(Ash.Type.Tuple,
          fields: [
            name: [type: :string],
            count: [type: :integer]
          ]
        )

      original = {"hello", 42}

      assert {:ok, dumped} = Ash.Type.dump_to_embedded(Ash.Type.Tuple, original, constraints)

      string_keyed = Map.new(dumped, fn {k, v} -> {to_string(k), v} end)

      assert {:ok, restored} =
               Ash.Type.cast_from_embedded(Ash.Type.Tuple, string_keyed, constraints)

      assert restored == original
    end
  end
end
