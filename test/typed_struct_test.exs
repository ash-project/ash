defmodule Ash.TypedStructTest do
  use ExUnit.Case, async: true

  defmodule Thing do
    use Ash.TypedStruct

    typed_struct do
      field(:name, :string, allow_nil?: false, default: "foo")
    end
  end

  defmodule UserStruct do
    use Ash.TypedStruct

    typed_struct do
      field(:id, Ash.Type.UUID, allow_nil?: false)
      field(:name, :string, allow_nil?: false)
      field(:email, :string, constraints: [match: ~r/@/])
      field(:age, :integer, constraints: [min: 0, max: 150])
      field(:active, :boolean, default: true)
    end
  end

  defmodule ExampleResource do
    use Ash.Resource, domain: nil

    attributes do
      uuid_primary_key :id
      attribute :user, UserStruct
    end
  end

  describe "DSL" do
    test "struct DSL creates a proper struct type" do
      assert {:ok, struct} =
               UserStruct.new(%{
                 id: "123e4567-e89b-12d3-a456-426614174000",
                 name: "John Doe",
                 email: "john@example.com",
                 age: 30
               })

      assert %UserStruct{
               id: "123e4567-e89b-12d3-a456-426614174000",
               name: "John Doe",
               email: "john@example.com",
               age: 30,
               active: true
             } = struct
    end

    test "struct DSL validates email format" do
      assert {:error, _} =
               UserStruct.new(%{
                 id: "123e4567-e89b-12d3-a456-426614174000",
                 name: "John Doe",
                 email: "invalid-email",
                 age: 30
               })
    end

    test "struct DSL validates age constraints" do
      assert {:error, _} =
               UserStruct.new(%{
                 id: "123e4567-e89b-12d3-a456-426614174000",
                 name: "John Doe",
                 email: "john@example.com",
                 age: 200
               })
    end

    test "struct DSL enforces allow_nil? false" do
      assert {:error, _} =
               UserStruct.new(%{
                 id: "123e4567-e89b-12d3-a456-426614174000",
                 email: "john@example.com"
               })
    end

    test "struct DSL applies defaults" do
      assert {:ok, struct} =
               UserStruct.new(%{
                 id: "123e4567-e89b-12d3-a456-426614174000",
                 name: "John Doe",
                 email: "john@example.com",
                 age: 30,
                 active: false
               })

      assert struct.active == false
    end

    test "new! raises on errors" do
      assert_raise Ash.Error.Invalid, fn ->
        UserStruct.new!(%{
          id: "123e4567-e89b-12d3-a456-426614174000",
          name: "John Doe",
          email: "invalid-email",
          age: 30
        })
      end
    end

    test "Thing struct uses default value" do
      assert {:ok, thing} = Thing.new(%{})
      assert thing.name == "foo"
    end
  end
end
