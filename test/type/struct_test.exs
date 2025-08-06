defmodule Type.StructTest do
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule Metadata do
    defstruct [:foo, :bar]
  end

  defmodule Embedded do
    use Ash.Resource, data_layer: :embedded

    attributes do
      attribute :name, :string, allow_nil?: false, public?: true
      attribute :title, :string, allow_nil?: false, public?: true
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
               message: "must be more than or equal to %{min}",
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
          bar: -1  # constraint violation (min: 0)
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
      error.field == :bar && error.message == "must be more than or equal to %{min}"
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
                        email: [type: :string, allow_nil?: false, constraints: [match: ~r/@/]]
                      ]
        end
      end
    end

    changeset =
      ComplexPost
      |> Ash.Changeset.for_create(:create, %{
        complex_metadata: %{
          name: "ab",      # too short (min_length: 3)
          age: -5,         # negative (min: 0)
          email: "invalid" # no @ symbol
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
      error.field == :age && String.contains?(error.message, "more than")
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
        assert Enum.any?(error_messages, &String.contains?(&1, "more than"))

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
               vars: [],
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
end
