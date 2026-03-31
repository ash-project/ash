# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Type.MapTest do
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain
  alias Ash.Test.DumpTestType

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

      attribute :metadata, :map do
        public? true

        constraints fields: [
                      foo: [type: :string, allow_nil?: false],
                      integer_min_0: [type: :integer, constraints: [min: 0]],
                      string_min_3: [
                        type: :string,
                        allow_nil?: true,
                        constraints: [min_length: 3]
                      ],
                      string_max_3: [
                        type: :string,
                        allow_nil?: true,
                        constraints: [max_length: 3]
                      ],
                      string_match: [
                        type: :string,
                        allow_nil?: true,
                        constraints: [match: {"a_A", ""}]
                      ]
                    ]
      end

      attribute :otherdata, :map do
        public? true
      end
    end
  end

  defmodule MapWithFields do
    use Ash.Type.NewType,
      subtype_of: :map,
      constraints: [
        fields: [
          foo: [type: :string, allow_nil?: false]
        ]
      ]
  end

  test "cast_stored honors string keys" do
    {:ok, constraints} = Ash.Type.init(MapWithFields, [])

    assert {:ok, %{foo: "bar"}} =
             Ash.Type.cast_stored(MapWithFields, %{"foo" => "bar"}, constraints)
  end

  test "cast_stored does not cast keys with nil values by default" do
    {:ok, constraints} = Ash.Type.init(Ash.Type.Map, fields: [foo: [type: :string]])

    assert {:ok, %{}} = Ash.Type.cast_stored(Ash.Type.Map, %{"foo" => nil}, constraints)
  end

  test "cast_input casts keys with nil values by default" do
    {:ok, constraints} = Ash.Type.init(Ash.Type.Map, fields: [foo: [type: :string]])

    assert {:ok, %{"foo" => nil}} =
             Ash.Type.cast_input(Ash.Type.Map, %{"foo" => nil}, constraints)
  end

  test "cast_stored casts keys with nil values when preserve_nil_values? is true" do
    {:ok, constraints} =
      Ash.Type.init(Ash.Type.Map, fields: [foo: [type: :string]], preserve_nil_values?: true)

    assert {:ok, %{foo: nil}} = Ash.Type.cast_stored(Ash.Type.Map, %{"foo" => nil}, constraints)
  end

  test "it handles valid maps" do
    changeset =
      Post
      |> Ash.Changeset.for_create(:create, %{
        metadata: %{
          foo: "bar",
          integer_min_0: 1
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
          integer_min_0: "2"
        }
      })

    assert changeset.valid?

    assert changeset.attributes == %{
             metadata: %{foo: "bar", integer_min_0: 2}
           }
  end

  test "cast result has only atom keys" do
    changeset =
      Post
      |> Ash.Changeset.for_create(:create, %{
        metadata: %{
          "integer_min_0" => nil,
          foo: "bar"
        }
      })

    assert changeset.valid?

    assert changeset.attributes == %{
             metadata: %{foo: "bar", integer_min_0: nil}
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
        metadata: %{integer_min_0: 1}
      })

    refute changeset.valid?

    assert [
             %Ash.Error.Changes.InvalidAttribute{
               field: :foo,
               message: "field must be present",
               private_vars: nil,
               value: %{integer_min_0: 1},
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
        metadata: %{foo: "hello", integer_min_0: -1}
      })

    refute changeset.valid?

    assert [
             %Ash.Error.Changes.InvalidAttribute{
               field: :integer_min_0,
               message: "must be greater than or equal to %{min}",
               private_vars: nil,
               value: %{integer_min_0: -1, foo: "hello"},
               bread_crumbs: [],
               vars: [min: 0],
               path: [:metadata]
             }
           ] = changeset.errors
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
             metadata: %{foo: "bar"}
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
            integer_min_0: "2"
          }
        }
      )

    refute changeset.valid?

    assert [
             %Ash.Error.Changes.InvalidAttribute{
               field: :foo,
               message: "value must not be nil",
               private_vars: nil,
               value: %{:integer_min_0 => "2", "foo" => ""},
               bread_crumbs: [],
               vars: [],
               path: [:metadata]
             }
           ] = changeset.errors
  end

  test "maps without constraints are left as is" do
    changeset =
      Post
      |> Ash.Changeset.for_create(
        :create,
        %{
          otherdata: %{
            extra: "field"
          }
        }
      )

    assert changeset.valid?

    assert changeset.attributes == %{
             otherdata: %{extra: "field"}
           }
  end

  test "multiple values with constraint errors" do
    changeset =
      Post
      |> Ash.Changeset.for_create(:create, %{
        metadata: %{
          foo: "bar",
          string_min_3: "a",
          integer_min_0: -1
        }
      })

    refute changeset.valid?

    # Now returns both errors as expected
    errors = changeset.errors
    assert length(errors) == 2

    # Check that we have both errors
    assert Enum.any?(errors, fn error ->
             error.field == :integer_min_0 && String.contains?(error.message, "greater than")
           end)

    assert Enum.any?(errors, fn error ->
             error.field == :string_min_3 && String.contains?(error.message, "length")
           end)
  end

  test "returns multiple field errors simultaneously for maps" do
    # This test demonstrates that we want ALL field errors returned at once
    # Currently this will fail because only the first error is returned
    changeset =
      Post
      |> Ash.Changeset.for_create(:create, %{
        metadata: %{
          # foo is missing (required field)
          # constraint violation (min: 0)
          integer_min_0: -1,
          # constraint violation (min_length: 3)
          string_min_3: "a"
        }
      })

    refute changeset.valid?

    # We expect ALL errors to be returned, not just one
    errors = changeset.errors
    assert length(errors) >= 2

    # Check that we have both a missing field error and constraint errors
    assert Enum.any?(errors, fn error ->
             error.field == :foo && error.message == "field must be present"
           end)

    assert Enum.any?(errors, fn error ->
             error.field == :integer_min_0 && String.contains?(error.message, "greater than")
           end)

    # We should also get the string length error
    assert Enum.any?(errors, fn error ->
             error.field == :string_min_3 && String.contains?(error.message, "length")
           end)
  end

  test "returns all constraint violations across multiple fields in maps" do
    # Test that all constraint violations are reported simultaneously
    changeset =
      Post
      |> Ash.Changeset.for_create(:create, %{
        metadata: %{
          # valid
          foo: "bar",
          # invalid: below minimum
          integer_min_0: -5,
          # invalid: too short
          string_min_3: "ab",
          # invalid: too long
          string_max_3: "abcde",
          # invalid: doesn't match pattern
          string_match: "xyz"
        }
      })

    refute changeset.valid?

    # We expect ALL four constraint violations to be returned
    errors = changeset.errors
    assert length(errors) >= 4

    # Check for integer minimum error
    assert Enum.any?(errors, fn error ->
             error.field == :integer_min_0 && String.contains?(error.message, "greater than")
           end)

    # Check for string minimum length error
    assert Enum.any?(errors, fn error ->
             error.field == :string_min_3 && String.contains?(error.message, "greater than")
           end)

    # Check for string maximum length error
    assert Enum.any?(errors, fn error ->
             error.field == :string_max_3 && String.contains?(error.message, "less than")
           end)

    # Check for string match error
    assert Enum.any?(errors, fn error ->
             error.field == :string_match && String.contains?(error.message, "match")
           end)
  end

  test "direct map type casting returns multiple errors" do
    # Test the type casting behavior directly, not through changeset
    constraints = [
      fields: [
        foo: [type: :string, allow_nil?: false],
        bar: [type: :integer, constraints: [min: 0]],
        baz: [type: :string, constraints: [min_length: 5]]
      ]
    ]

    # Test with missing required field AND multiple constraint violations
    case Ash.Type.apply_constraints(Ash.Type.Map, %{bar: -1, baz: "abc"}, constraints) do
      {:error, errors} ->
        # We expect multiple errors to be present
        error_list = List.wrap(errors)
        assert length(error_list) >= 3

        # Check for all error types
        error_messages =
          error_list
          |> Enum.map(&(&1[:message] || to_string(&1)))

        assert Enum.any?(error_messages, &String.contains?(&1, "must be present"))
        assert Enum.any?(error_messages, &String.contains?(&1, "greater than"))
        assert Enum.any?(error_messages, &String.contains?(&1, "length"))

      {:ok, _} ->
        flunk("Expected validation errors but got success")
    end
  end

  test "string_min_3 validates length" do
    changeset =
      Post
      |> Ash.Changeset.for_create(:create, %{
        metadata: %{
          foo: "bar",
          string_min_3: "a"
        }
      })

    refute changeset.valid?

    assert [
             %Ash.Error.Changes.InvalidAttribute{
               message: "length must be greater than or equal to %{min}",
               vars: [min: 3],
               field: :string_min_3,
               private_vars: nil,
               value: %{string_min_3: "a", foo: "bar"},
               bread_crumbs: [],
               path: [:metadata]
             }
           ] = changeset.errors
  end

  test "string_max_3 validates length" do
    changeset =
      Post
      |> Ash.Changeset.for_create(:create, %{
        metadata: %{
          foo: "bar",
          string_max_3: "aaaa"
        }
      })

    refute changeset.valid?

    assert [
             %Ash.Error.Changes.InvalidAttribute{
               message: "length must be less than or equal to %{max}",
               vars: [max: 3],
               field: :string_max_3,
               private_vars: nil,
               value: %{string_max_3: "aaaa", foo: "bar"},
               bread_crumbs: [],
               path: [:metadata]
             }
           ] = changeset.errors
  end

  describe "dump_to_native" do
    test "recursively dumps field types" do
      {:ok, constraints} =
        Ash.Type.init(Ash.Type.Map,
          fields: [
            name: [type: DumpTestType],
            count: [type: :integer]
          ]
        )

      assert {:ok, %{name: "native:hello", count: 42}} =
               Ash.Type.dump_to_native(Ash.Type.Map, %{name: "hello", count: 42}, constraints)
    end

    test "drops extra keys not in field definitions" do
      {:ok, constraints} =
        Ash.Type.init(Ash.Type.Map,
          fields: [
            name: [type: :string]
          ]
        )

      assert {:ok, %{name: "hello"}} =
               Ash.Type.dump_to_native(
                 Ash.Type.Map,
                 %{name: "hello", extra: "dropped"},
                 constraints
               )
    end

    test "without fields returns map as-is" do
      assert {:ok, %{anything: "goes"}} =
               Ash.Type.dump_to_native(Ash.Type.Map, %{anything: "goes"}, [])
    end

    test "handles nil" do
      assert {:ok, nil} = Ash.Type.dump_to_native(Ash.Type.Map, nil, [])
    end

    test "handles missing fields gracefully" do
      {:ok, constraints} =
        Ash.Type.init(Ash.Type.Map,
          fields: [
            name: [type: :string],
            age: [type: :integer]
          ]
        )

      assert {:ok, %{name: "hello"}} =
               Ash.Type.dump_to_native(Ash.Type.Map, %{name: "hello"}, constraints)
    end

    test "respects preserve_nil_values? true" do
      {:ok, constraints} =
        Ash.Type.init(Ash.Type.Map,
          fields: [name: [type: :string]],
          preserve_nil_values?: true
        )

      assert {:ok, %{name: nil}} =
               Ash.Type.dump_to_native(Ash.Type.Map, %{name: nil}, constraints)
    end

    test "respects preserve_nil_values? false" do
      {:ok, constraints} =
        Ash.Type.init(Ash.Type.Map,
          fields: [name: [type: :string]],
          preserve_nil_values?: false
        )

      assert {:ok, %{}} =
               Ash.Type.dump_to_native(Ash.Type.Map, %{name: nil}, constraints)
    end

    test "honors string keys in field lookup" do
      {:ok, constraints} =
        Ash.Type.init(Ash.Type.Map,
          fields: [
            name: [type: DumpTestType]
          ]
        )

      assert {:ok, %{name: "native:hello"}} =
               Ash.Type.dump_to_native(Ash.Type.Map, %{"name" => "hello"}, constraints)
    end

    test "returns error for non-map values" do
      assert :error = Ash.Type.dump_to_native(Ash.Type.Map, "string", [])
      assert :error = Ash.Type.dump_to_native(Ash.Type.Map, 123, [])
    end
  end

  describe "dump_to_embedded" do
    test "recursively calls dump_to_embedded on field types" do
      {:ok, constraints} =
        Ash.Type.init(Ash.Type.Map,
          fields: [
            name: [type: DumpTestType],
            count: [type: :integer]
          ]
        )

      assert {:ok, %{name: "embedded:hello", count: 42}} =
               Ash.Type.dump_to_embedded(Ash.Type.Map, %{name: "hello", count: 42}, constraints)
    end

    test "uses dump_to_embedded not dump_to_native on fields" do
      {:ok, constraints} =
        Ash.Type.init(Ash.Type.Map,
          fields: [
            val: [type: DumpTestType]
          ]
        )

      {:ok, native_result} =
        Ash.Type.dump_to_native(Ash.Type.Map, %{val: "test"}, constraints)

      {:ok, embedded_result} =
        Ash.Type.dump_to_embedded(Ash.Type.Map, %{val: "test"}, constraints)

      assert native_result[:val] == "native:test"
      assert embedded_result[:val] == "embedded:test"
    end

    test "without fields returns map as-is" do
      assert {:ok, %{anything: "goes"}} =
               Ash.Type.dump_to_embedded(Ash.Type.Map, %{anything: "goes"}, [])
    end

    test "handles nil" do
      assert {:ok, nil} = Ash.Type.dump_to_embedded(Ash.Type.Map, nil, [])
    end

    test "drops extra keys not in field definitions" do
      {:ok, constraints} =
        Ash.Type.init(Ash.Type.Map,
          fields: [name: [type: :string]]
        )

      assert {:ok, %{name: "hello"}} =
               Ash.Type.dump_to_embedded(
                 Ash.Type.Map,
                 %{name: "hello", extra: "dropped"},
                 constraints
               )
    end

    test "respects preserve_nil_values?" do
      {:ok, constraints} =
        Ash.Type.init(Ash.Type.Map,
          fields: [name: [type: :string]],
          preserve_nil_values?: true
        )

      assert {:ok, %{name: nil}} =
               Ash.Type.dump_to_embedded(Ash.Type.Map, %{name: nil}, constraints)

      {:ok, constraints_no_nil} =
        Ash.Type.init(Ash.Type.Map,
          fields: [name: [type: :string]],
          preserve_nil_values?: false
        )

      assert {:ok, %{}} =
               Ash.Type.dump_to_embedded(Ash.Type.Map, %{name: nil}, constraints_no_nil)
    end

    test "honors string keys in field lookup" do
      {:ok, constraints} =
        Ash.Type.init(Ash.Type.Map,
          fields: [name: [type: DumpTestType]]
        )

      assert {:ok, %{name: "embedded:hello"}} =
               Ash.Type.dump_to_embedded(Ash.Type.Map, %{"name" => "hello"}, constraints)
    end

    test "returns error for non-map values" do
      assert :error = Ash.Type.dump_to_embedded(Ash.Type.Map, "string", [])
    end
  end

  describe "dump/cast round-trip" do
    test "dump_to_native then cast_stored preserves data" do
      {:ok, constraints} =
        Ash.Type.init(Ash.Type.Map,
          fields: [
            name: [type: :string],
            count: [type: :integer]
          ]
        )

      original = %{name: "hello", count: 42}

      assert {:ok, dumped} = Ash.Type.dump_to_native(Ash.Type.Map, original, constraints)
      assert {:ok, restored} = Ash.Type.cast_stored(Ash.Type.Map, dumped, constraints)
      assert restored == original
    end

    test "dump_to_native then cast_stored with string keys round-trips" do
      {:ok, constraints} =
        Ash.Type.init(Ash.Type.Map,
          fields: [
            name: [type: :string],
            count: [type: :integer]
          ]
        )

      original = %{name: "hello", count: 42}

      assert {:ok, dumped} = Ash.Type.dump_to_native(Ash.Type.Map, original, constraints)

      # Simulate stored data having string keys (common with JSON storage)
      string_keyed = Map.new(dumped, fn {k, v} -> {to_string(k), v} end)

      assert {:ok, restored} = Ash.Type.cast_stored(Ash.Type.Map, string_keyed, constraints)
      assert restored == original
    end
  end

  test "string_match validates against regex pattern" do
    changeset =
      Post
      |> Ash.Changeset.for_create(:create, %{
        metadata: %{
          foo: "bar",
          string_match: "invalid"
        }
      })

    refute changeset.valid?

    assert [
             %Ash.Error.Changes.InvalidAttribute{
               message: "must match the pattern %{regex}",
               vars: [regex: "~r/a_A/"],
               field: :string_match,
               private_vars: nil,
               value: %{string_match: "invalid", foo: "bar"},
               bread_crumbs: [],
               path: [:metadata]
             }
           ] = changeset.errors
  end
end
