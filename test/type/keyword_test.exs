# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Type.KeywordTest do
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

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

      attribute :metadata, :keyword do
        public?(true)

        constraints fields: [
                      foo: [type: :string, allow_nil?: false],
                      bar: [type: :integer, constraints: [min: 0]]
                    ]
      end
    end
  end

  test "it handles valid maps" do
    changeset =
      Post
      |> Ash.Changeset.for_create(:create, %{
        metadata: [
          foo: "bar",
          bar: 1
        ]
      })

    assert changeset.valid?
  end

  test "allow_nil? is true by default" do
    changeset =
      Post
      |> Ash.Changeset.for_create(:create, %{
        metadata: [
          foo: "bar",
          bar: "2"
        ]
      })

    assert changeset.valid?

    assert changeset.attributes == %{
             metadata: [foo: "bar", bar: 2]
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
             metadata: [foo: "bar", bar: nil]
           }
  end

  test "keys that can be nil don't need to be there" do
    changeset =
      Post
      |> Ash.Changeset.for_create(:create, %{
        metadata: [
          foo: "bar"
        ]
      })

    assert changeset.valid?
  end

  test "keys that can not be nil need to be there" do
    changeset =
      Post
      |> Ash.Changeset.for_create(:create, %{
        metadata: [bar: 1]
      })

    refute changeset.valid?

    assert [
             %Ash.Error.Changes.InvalidAttribute{
               field: :foo,
               message: "field must be present",
               private_vars: nil,
               value: [bar: 1],
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
        metadata: [foo: "hello", bar: -1]
      })

    refute changeset.valid?

    assert [
             %Ash.Error.Changes.InvalidAttribute{
               field: :bar,
               message: "must be more than or equal to %{min}",
               private_vars: nil,
               value: [foo: "hello", bar: -1],
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
          metadata: [
            foo: "bar",
            extra: "field"
          ]
        }
      )

    assert changeset.valid?

    assert changeset.attributes == %{
             metadata: [foo: "bar"]
           }
  end

  test "values are casted before checked" do
    changeset =
      Post
      |> Ash.Changeset.for_create(
        :create,
        %{
          metadata: [
            foo: "",
            bar: "2"
          ]
        }
      )

    refute changeset.valid?

    assert [
             %Ash.Error.Changes.InvalidAttribute{
               field: :foo,
               message: "value must not be nil",
               private_vars: nil,
               value: [foo: "", bar: "2"],
               bread_crumbs: [],
               vars: [],
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
        metadata: [
          # foo is missing (required field)
          # constraint violation (min: 0)
          bar: -1
        ]
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
             error.field == :bar && error.message == "must be more than or equal to %{min}"
           end)
  end

  test "returns multiple constraint violations for different fields" do
    # Test multiple constraint violations across different fields
    defmodule ComplexPost do
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

        attribute :complex_metadata, :keyword do
          public? true

          constraints fields: [
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
        complex_metadata: [
          # too short (min_length: 3)
          name: "ab",
          # negative (min: 0)
          age: -5,
          # no @ symbol
          email: "invalid"
        ]
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
             error.field == :age && String.contains?(error.message, "more than")
           end)

    # Check for email format error
    assert Enum.any?(errors, fn error ->
             error.field == :email && String.contains?(error.message, "match")
           end)
  end

  test "returns multiple errors for mixed scenarios" do
    # Test combination of missing fields, type casting errors, and constraint violations
    defmodule MixedPost do
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

        attribute :mixed_metadata, :keyword do
          public? true

          constraints fields: [
                        required_string: [type: :string, allow_nil?: false],
                        positive_int: [type: :integer, constraints: [min: 1]],
                        valid_email: [
                          type: :string,
                          allow_nil?: false,
                          constraints: [match: {"@", ""}]
                        ],
                        optional_field: [type: :string, allow_nil?: true]
                      ]
        end
      end
    end

    changeset =
      MixedPost
      |> Ash.Changeset.for_create(:create, %{
        mixed_metadata: [
          # required_string is missing entirely
          # constraint violation (min: 1)
          positive_int: 0,
          # constraint violation (no @)
          valid_email: "no-at-sign"
          # optional_field is missing but that's OK
        ]
      })

    refute changeset.valid?

    # We expect all three errors to be returned
    errors = changeset.errors
    assert length(errors) == 3

    # Check for missing required field
    assert Enum.any?(errors, fn error ->
             error.field == :required_string && error.message == "field must be present"
           end)

    # Check for positive integer constraint
    assert Enum.any?(errors, fn error ->
             error.field == :positive_int && String.contains?(error.message, "more than")
           end)

    # Check for email format constraint
    assert Enum.any?(errors, fn error ->
             error.field == :valid_email && String.contains?(error.message, "match")
           end)
  end

  test "direct type casting returns multiple errors" do
    # Test the type casting behavior directly, not through changeset
    constraints = [
      fields: [
        foo: [type: :string, allow_nil?: false],
        bar: [type: :integer, constraints: [min: 0]]
      ]
    ]

    # Test with missing required field AND constraint violation
    case Ash.Type.apply_constraints(Ash.Type.Keyword, [bar: -1], constraints) do
      {:error, errors} ->
        # We expect both errors to be present
        assert length(List.wrap(errors)) >= 2

        # Check for both error types
        error_messages =
          errors
          |> List.wrap()
          |> Enum.map(&(&1[:message] || to_string(&1)))

        assert Enum.any?(error_messages, &String.contains?(&1, "must be present"))
        assert Enum.any?(error_messages, &String.contains?(&1, "more than"))

      {:ok, _} ->
        flunk("Expected validation errors but got success")
    end
  end

  test "direct type casting with multiple constraint violations" do
    # Test direct casting with multiple different constraint types
    constraints = [
      fields: [
        name: [type: :string, allow_nil?: false, constraints: [min_length: 5]],
        count: [type: :integer, constraints: [min: 10, max: 100]],
        email: [type: :string, constraints: [min_length: 5]]
      ]
    ]

    # Multiple violations: short name, low count, short email
    case Ash.Type.apply_constraints(
           Ash.Type.Keyword,
           [name: "abc", count: 5, email: "bad"],
           constraints
         ) do
      {:error, errors} ->
        # We expect at least 3 errors
        wrapped_errors = List.wrap(errors)
        assert length(wrapped_errors) >= 3

        error_messages = Enum.map(wrapped_errors, &(&1[:message] || to_string(&1)))

        # Check for name length constraint
        assert Enum.any?(error_messages, &String.contains?(&1, "length"))

        # Check for count minimum constraint
        assert Enum.any?(error_messages, &String.contains?(&1, "more than"))

        # Check for email length constraint
        assert Enum.any?(error_messages, &String.contains?(&1, "length"))

      {:ok, _} ->
        flunk("Expected validation errors but got success")
    end
  end

  test "accumulates errors for type casting failures and missing fields" do
    # Test that type casting failures are also accumulated with other errors
    defmodule TypeCastPost do
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

        attribute :cast_metadata, :keyword do
          public? true

          constraints fields: [
                        required_field: [type: :string, allow_nil?: false],
                        integer_field: [type: :integer, allow_nil?: false],
                        boolean_field: [type: :boolean, constraints: []]
                      ]
        end
      end
    end

    changeset =
      TypeCastPost
      |> Ash.Changeset.for_create(:create, %{
        cast_metadata: [
          # required_field is missing
          # type casting will fail
          integer_field: "not_an_integer",
          # type casting will fail
          boolean_field: "not_a_boolean"
        ]
      })

    refute changeset.valid?

    # We expect multiple errors: missing field + type casting failures
    errors = changeset.errors
    assert length(errors) >= 2

    # Check for missing field error
    assert Enum.any?(errors, fn error ->
             error.field == :required_field && error.message == "field must be present"
           end)

    # There should be additional errors for type casting failures
    # The exact error messages may vary but we should have more than just the missing field
    assert length(errors) > 1
  end

  test "dump_to_native converts keyword list to map" do
    keyword_list = [foo: "bar", baz: 42]
    assert Ash.Type.Keyword.dump_to_native(keyword_list, []) == {:ok, %{foo: "bar", baz: 42}}
  end

  test "dump_to_native handles nil" do
    assert Ash.Type.Keyword.dump_to_native(nil, []) == {:ok, nil}
  end

  test "dump_to_native handles maps" do
    map = %{foo: "bar", baz: 42}
    assert Ash.Type.Keyword.dump_to_native(map, []) == {:ok, map}
  end

  test "dump_to_native returns error for invalid input" do
    assert Ash.Type.Keyword.dump_to_native("invalid", []) == :error
    assert Ash.Type.Keyword.dump_to_native(123, []) == :error
  end
end
