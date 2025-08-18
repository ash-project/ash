defmodule Ash.TypedStructTest do
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

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

      field(:email, :string,
        constraints: [match: ~r/@/],
        description: "the user's email address, must contain a @"
      )

      field(:age, :integer, constraints: [min: 0, max: 150])
      field(:active, :boolean, default: true)
    end
  end

  defmodule ExampleResource do
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
      attribute :user, UserStruct, public?: true
    end
  end

  describe "typed struct DSL" do
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

    test "new! returns struct when all required fields are present" do
      assert %Ash.TypedStructTest.UserStruct{
               id: "123e4567-e89b-12d3-a456-426614174000",
               name: "John Doe",
               email: "john@example.com",
               age: 30
             } =
               UserStruct.new!(%{
                 id: "123e4567-e89b-12d3-a456-426614174000",
                 name: "John Doe",
                 email: "john@example.com",
                 age: 30
               })
    end

    test "new! raises when required fields with no default are missing" do
      assert_raise Ash.Error.Invalid, fn ->
        UserStruct.new!(%{
          id: "123e4567-e89b-12d3-a456-426614174000",
          email: "invalid-email",
          age: 30
        })
      end
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

  describe "typed struct as resource attribute" do
    test "it handles valid maps" do
      changeset =
        ExampleResource
        |> Ash.Changeset.for_create(:create, %{
          user: %{
            id: "a7cec9ba-15de-4c56-99e4-c2abc91a2209",
            name: "bar"
          }
        })

      assert changeset.valid?
    end

    test "it handles missing maps" do
      changeset =
        ExampleResource
        |> Ash.Changeset.for_create(:create, %{})

      assert changeset.valid?
    end

    test "it handles nil maps" do
      changeset =
        ExampleResource
        |> Ash.Changeset.for_create(:create, %{
          user: nil
        })

      assert changeset.valid?
    end
  end

  describe "typed struct introspection" do
    test "short name can be introspected" do
      assert Ash.TypedStruct.Info.short_name(UserStruct) == :user_struct
    end

    test "field names can be introspected in order" do
      assert Ash.TypedStruct.Info.field_names(UserStruct) == [:id, :name, :email, :age, :active]
    end

    test "all fields structs can be introspected" do
      fields = Ash.TypedStruct.Info.fields(UserStruct)
      assert length(fields) == 5

      Enum.each(
        fields,
        fn field ->
          assert is_struct(field, Ash.TypedStruct.Field)
        end
      )
    end

    test "field struct can be introspected by name" do
      field = Ash.TypedStruct.Info.field(UserStruct, :email)
      assert is_struct(field, Ash.TypedStruct.Field)
    end

    test "extensions can be introspected" do
      assert Ash.TypedStruct.Info.extensions(UserStruct) == [Ash.TypedStruct.Dsl]
    end
  end
end
