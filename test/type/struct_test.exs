# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Type.StructTest do
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain
  alias Ash.Test.DumpTestType

  defmodule Metadata do
    defstruct [:foo, :bar, not_nil_by_default: "foo"]
  end

  defmodule Embedded do
    use Ash.Resource, data_layer: :embedded

    attributes do
      attribute :name, :string, allow_nil?: false, public?: true
      attribute :title, :string, allow_nil?: false, public?: true
    end
  end

  defmodule EmbeddedWithTuple do
    use Ash.Resource, data_layer: :embedded

    attributes do
      attribute :name, :string, allow_nil?: false, public?: true

      attribute :metadata, :tuple do
        allow_nil? true
        public? true
        constraints fields: [key: [type: :string]]
      end
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, update: :*]

      create :create do
        primary? true
        accept [:*]

        argument :dummy_metadata, :struct, constraints: [instance_of: Metadata], allow_nil?: true
      end
    end

    attributes do
      uuid_primary_key :id

      attribute :metadata, :struct do
        public? true

        constraints instance_of: Metadata,
                    fields: [
                      foo: [type: :string, allow_nil?: false],
                      bar: [type: :integer, constraints: [min: 0]]
                    ]
      end
    end
  end

  defmodule InvalidPost do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, update: :*]

      create :create do
        primary? true
        accept [:*]

        argument :dummy_metadata, :struct,
          constraints: [instance_of: NonExistingModule],
          allow_nil?: true
      end
    end

    attributes do
      uuid_primary_key :id
    end
  end

  test "an embedded resource can be used" do
    assert {:ok, %Embedded{name: "fred", title: "title"}} =
             Ash.Type.apply_constraints(Ash.Type.Struct, %{"name" => "fred", :title => "title"},
               instance_of: Embedded
             )
  end

  test "cast_stored does not cast keys with nil values by default" do
    constraints = [
      instance_of: Metadata,
      fields: [
        not_nil_by_default: [type: :string]
      ]
    ]

    assert {:ok, %Metadata{not_nil_by_default: "foo"}} =
             Ash.Type.cast_stored(Ash.Type.Struct, %{"not_nil_by_default" => nil}, constraints)
  end

  test "cast_stored casts keys with nil values by default" do
    constraints = [
      instance_of: Metadata,
      preserve_nil_values?: true,
      fields: [
        not_nil_by_default: [type: :string]
      ]
    ]

    assert {:ok, %Metadata{not_nil_by_default: nil}} =
             Ash.Type.cast_stored(Ash.Type.Struct, %{"not_nil_by_default" => nil}, constraints)
  end

  test "it handles valid maps" do
    changeset =
      Post
      |> Ash.Changeset.for_create(:create, %{
        metadata: %{
          foo: "bar",
          bar: 1
        }
      })

    assert changeset.valid?
  end

  test "it handles missing maps" do
    changeset =
      Post
      |> Ash.Changeset.for_create(:create, %{})

    assert changeset.valid?
  end

  test "it handles nil maps" do
    changeset =
      Post
      |> Ash.Changeset.for_create(:create, %{
        metadata: nil
      })

    assert changeset.valid?
  end

  test "allow_nil? is true by default" do
    changeset =
      Post
      |> Ash.Changeset.for_create(:create, %{
        metadata: %{
          foo: "bar",
          bar: "2"
        }
      })

    assert changeset.valid?

    assert changeset.attributes == %{
             metadata: %Metadata{foo: "bar", bar: 2}
           }
  end

  test "cast result has only atom keys" do
    changeset =
      Post
      |> Ash.Changeset.for_create(:create, %{
        metadata: %{
          "bar" => nil,
          foo: "bar"
        }
      })

    assert changeset.valid?

    assert changeset.attributes == %{
             metadata: %Metadata{foo: "bar", bar: nil}
           }
  end

  test "keys that can be nil don't need to be there" do
    changeset =
      Post
      |> Ash.Changeset.for_create(:create, %{
        metadata: %{
          foo: "bar"
        }
      })

    assert changeset.valid?
  end

  test "keys that can not be nil need to be there" do
    changeset =
      Post
      |> Ash.Changeset.for_create(:create, %{
        metadata: %{bar: 1}
      })

    refute changeset.valid?

    assert [
             %Ash.Error.Changes.InvalidAttribute{
               field: :foo,
               message: "field must be present",
               private_vars: nil,
               value: %{bar: 1},
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
        metadata: %{foo: "hello", bar: -1}
      })

    refute changeset.valid?

    assert [
             %Ash.Error.Changes.InvalidAttribute{
               field: :bar,
               message: "must be greater than or equal to %{min}",
               private_vars: nil,
               value: %{bar: -1, foo: "hello"},
               bread_crumbs: [],
               vars: [min: 0],
               path: [:metadata]
             }
           ] = changeset.errors
  end

  test "returns multiple field errors simultaneously" do
    # This test demonstrates that we want ALL field errors returned at once
    # Currently this will fail because only the first error is returned
    changeset =
      Post
      |> Ash.Changeset.for_create(:create, %{
        metadata: %{
          # foo is missing (required field)
          # constraint violation (min: 0)
          bar: -1
        }
      })

    refute changeset.valid?

    # We expect BOTH errors to be returned, not just one
    errors = changeset.errors
    assert length(errors) == 2

    # Check that we have both a missing field error and a constraint error
    assert Enum.any?(errors, fn error ->
             error.field == :foo && error.message == "field must be present"
           end)

    assert Enum.any?(errors, fn error ->
             error.field == :bar && error.message == "must be greater than or equal to %{min}"
           end)
  end

  test "returns multiple constraint violations for different fields" do
    # Test multiple constraint violations across different fields
    # This requires a more complex struct definition
    defmodule ComplexMetadata do
      defstruct [:name, :age, :email]
    end

    defmodule ComplexPost do
      use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

      ets do
        private?(true)
      end

      actions do
        default_accept :*
        defaults [:read, :destroy, update: :*]

        create :create do
          primary? true
          accept [:*]
        end
      end

      attributes do
        uuid_primary_key :id

        attribute :complex_metadata, :struct do
          public? true

          constraints instance_of: ComplexMetadata,
                      fields: [
                        name: [type: :string, allow_nil?: false, constraints: [min_length: 3]],
                        age: [type: :integer, allow_nil?: false, constraints: [min: 0, max: 150]],
                        email: [type: :string, allow_nil?: false, constraints: [match: {"@", ""}]]
                      ]
        end
      end
    end

    changeset =
      ComplexPost
      |> Ash.Changeset.for_create(:create, %{
        complex_metadata: %{
          # too short (min_length: 3)
          name: "ab",
          # negative (min: 0)
          age: -5,
          # no @ symbol
          email: "invalid"
        }
      })

    refute changeset.valid?

    # We expect ALL three constraint violations to be returned
    errors = changeset.errors
    assert length(errors) == 3

    # Check for name length error
    assert Enum.any?(errors, fn error ->
             error.field == :name && String.contains?(error.message, "length")
           end)

    # Check for age minimum error
    assert Enum.any?(errors, fn error ->
             error.field == :age && String.contains?(error.message, "greater than")
           end)

    # Check for email format error
    assert Enum.any?(errors, fn error ->
             error.field == :email && String.contains?(error.message, "match")
           end)
  end

  test "direct type casting returns multiple errors" do
    # Test the type casting behavior directly, not through changeset
    constraints = [
      instance_of: Metadata,
      fields: [
        foo: [type: :string, allow_nil?: false],
        bar: [type: :integer, constraints: [min: 0]]
      ]
    ]

    # Test with missing required field AND constraint violation
    case Ash.Type.apply_constraints(Ash.Type.Struct, %{bar: -1}, constraints) do
      {:error, errors} ->
        # We expect both errors to be present
        assert length(List.wrap(errors)) >= 2

        # Check for both error types
        error_messages =
          errors
          |> List.wrap()
          |> Enum.map(&(&1[:message] || to_string(&1)))

        assert Enum.any?(error_messages, &String.contains?(&1, "must be present"))
        assert Enum.any?(error_messages, &String.contains?(&1, "greater than"))

      {:ok, _} ->
        flunk("Expected validation errors but got success")
    end
  end

  test "extra fields are removed" do
    changeset =
      Post
      |> Ash.Changeset.for_create(
        :create,
        %{
          metadata: %{
            "foo" => "bar",
            extra: "field"
          }
        }
      )

    assert changeset.valid?

    assert changeset.attributes == %{
             metadata: %Metadata{foo: "bar"}
           }
  end

  test "values are casted before checked" do
    changeset =
      Post
      |> Ash.Changeset.for_create(
        :create,
        %{
          metadata: %{
            "foo" => "",
            bar: "2"
          }
        }
      )

    refute changeset.valid?

    assert [
             %Ash.Error.Changes.InvalidAttribute{
               field: :foo,
               message: "value must not be nil",
               private_vars: nil,
               value: %{:bar => "2", "foo" => ""},
               bread_crumbs: [],
               vars: [{:value, nil}],
               path: [:metadata]
             }
           ] = changeset.errors
  end

  test "invalid instance of argument is checked" do
    changeset =
      InvalidPost
      |> Ash.Changeset.for_create(:create, %{dummy_metadata: %Metadata{}})

    refute changeset.valid?

    assert [
             %Ash.Error.Changes.InvalidArgument{
               field: :dummy_metadata,
               message: "is invalid",
               bread_crumbs: [],
               vars: [],
               path: []
             }
           ] = changeset.errors
  end

  test "it handles nil argument" do
    changeset =
      Post
      |> Ash.Changeset.for_create(:create, %{dummy_metadata: nil})

    assert changeset.valid?
  end

  test "instance_of with nullable tuple fields handles nil values" do
    # When a resource has a nullable tuple attribute and we use instance_of
    # without explicit fields, nil tuple values should not crash apply_constraints
    constraints = [instance_of: EmbeddedWithTuple]

    assert {:ok, %EmbeddedWithTuple{name: "test", metadata: nil}} =
             Ash.Type.apply_constraints(Ash.Type.Struct, %{name: "test"}, constraints)
  end

  test "instance_of auto-derived fields propagate allow_nil?" do
    # When using instance_of with an Ash resource and no explicit fields,
    # the auto-derived fields from attributes should include allow_nil?
    constraints = [instance_of: Embedded]

    # Missing required fields should be rejected
    assert {:error, _} = Ash.Type.apply_constraints(Ash.Type.Struct, %{}, constraints)

    # Providing required fields should succeed
    assert {:ok, %Embedded{name: "fred", title: "title"}} =
             Ash.Type.apply_constraints(
               Ash.Type.Struct,
               %{name: "fred", title: "title"},
               constraints
             )
  end

  test "apply_constraints preserves __meta__ state for already valid struct instances" do
    loaded_struct = %Embedded{name: "fred", title: "title"}
    loaded_struct = Ecto.put_meta(loaded_struct, state: :loaded)
    assert loaded_struct.__meta__.state == :loaded

    assert {:ok, result} =
             Ash.Type.apply_constraints(Ash.Type.Struct, loaded_struct, instance_of: Embedded)

    assert result.__meta__.state == :loaded
    assert result.name == "fred"
    assert result.title == "title"
  end

  describe "dump_to_embedded" do
    defmodule DumpMetadata do
      defstruct [:name, :val]
    end

    test "recursively calls dump_to_embedded on field types" do
      constraints = [
        instance_of: DumpMetadata,
        fields: [
          name: [type: :string],
          val: [type: DumpTestType]
        ]
      ]

      assert {:ok, %{name: "hello", val: "embedded:world"}} =
               Ash.Type.dump_to_embedded(
                 Ash.Type.Struct,
                 %DumpMetadata{name: "hello", val: "world"},
                 constraints
               )
    end

    test "uses dump_to_embedded not dump_to_native on fields" do
      constraints = [
        instance_of: DumpMetadata,
        fields: [
          val: [type: DumpTestType]
        ]
      ]

      value = %DumpMetadata{val: "test"}

      {:ok, native_result} = Ash.Type.dump_to_native(Ash.Type.Struct, value, constraints)
      {:ok, embedded_result} = Ash.Type.dump_to_embedded(Ash.Type.Struct, value, constraints)

      assert native_result[:val] == "native:test"
      assert embedded_result[:val] == "embedded:test"
    end

    test "handles nil" do
      assert {:ok, nil} =
               Ash.Type.dump_to_embedded(Ash.Type.Struct, nil, instance_of: DumpMetadata)
    end

    test "returns error without instance_of" do
      assert :error =
               Ash.Type.dump_to_embedded(
                 Ash.Type.Struct,
                 %DumpMetadata{name: "test"},
                 fields: [name: [type: :string]]
               )
    end

    test "returns error for non-map values" do
      assert :error =
               Ash.Type.dump_to_embedded(
                 Ash.Type.Struct,
                 "string",
                 instance_of: DumpMetadata
               )
    end

    test "handles missing fields gracefully" do
      constraints = [
        instance_of: DumpMetadata,
        fields: [
          name: [type: :string],
          val: [type: DumpTestType]
        ]
      ]

      assert {:ok, %{name: "hello"}} =
               Ash.Type.dump_to_embedded(
                 Ash.Type.Struct,
                 %DumpMetadata{name: "hello", val: nil},
                 constraints
               )
    end
  end

  describe "dump/cast round-trip" do
    test "dump_to_native then cast_stored preserves data" do
      constraints = [
        instance_of: Metadata,
        fields: [
          foo: [type: :string],
          bar: [type: :integer]
        ]
      ]

      original = %Metadata{foo: "hello", bar: 42}

      assert {:ok, dumped} = Ash.Type.dump_to_native(Ash.Type.Struct, original, constraints)
      assert {:ok, restored} = Ash.Type.cast_stored(Ash.Type.Struct, dumped, constraints)
      assert restored.foo == original.foo
      assert restored.bar == original.bar
    end

    test "dump_to_native then cast_stored with string keys round-trips" do
      constraints = [
        instance_of: Metadata,
        fields: [
          foo: [type: :string],
          bar: [type: :integer]
        ]
      ]

      original = %Metadata{foo: "hello", bar: 42}

      assert {:ok, dumped} = Ash.Type.dump_to_native(Ash.Type.Struct, original, constraints)

      # Simulate stored data having string keys
      string_keyed = Map.new(dumped, fn {k, v} -> {to_string(k), v} end)

      assert {:ok, restored} = Ash.Type.cast_stored(Ash.Type.Struct, string_keyed, constraints)
      assert restored.foo == original.foo
      assert restored.bar == original.bar
    end
  end
end
