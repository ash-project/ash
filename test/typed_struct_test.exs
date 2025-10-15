# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.TypedStructTest do
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule Thing do
    use Ash.TypedStruct

    typed_struct do
      field(:name, :string, allow_nil?: false, default: "foo")
    end
  end

  defmodule StructWithListDefault do
    use Ash.TypedStruct

    typed_struct do
      field(:id, :string, allow_nil?: false)
      field(:tags, {:array, :string}, default: [])
    end
  end

  defmodule StructWithMapDefault do
    use Ash.TypedStruct

    typed_struct do
      field(:id, :string, allow_nil?: false)
      field(:metadata, :map, default: %{})
    end
  end

  defmodule StructWithNonEmptyDefaults do
    use Ash.TypedStruct

    typed_struct do
      field(:id, :string, allow_nil?: false)
      field(:tags, {:array, :string}, default: ["default", "tag"])
      field(:config, :map, default: %{key: "value"})
    end
  end

  defmodule StructWithMixedDefaults do
    use Ash.TypedStruct

    typed_struct do
      field(:name, :string, default: "John")
      field(:age, :integer, default: 30)
      field(:active, :boolean, default: true)
      field(:tags, {:array, :string}, default: [])
      field(:metadata, :map, default: %{})
      field(:options, {:array, :atom}, default: [:option1, :option2])
    end
  end

  defmodule StructWithNestedCollections do
    use Ash.TypedStruct

    typed_struct do
      field(:id, :string, allow_nil?: false)
      field(:nested_list, {:array, {:array, :string}}, default: [[]])
      field(:list_of_maps, {:array, :map}, default: [%{}])
      field(:map_with_defaults, :map, default: %{nested: %{key: "value"}})
    end
  end

  defmodule Reward do
    use Ash.TypedStruct

    typed_struct do
      field(:name, :string)
    end
  end

  defmodule Balance do
    use Ash.TypedStruct

    typed_struct do
      field(:points_balance, :float)
      field(:rewards, {:array, Reward}, default: [])
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

  defmodule ResourceWithCollectionStruct do
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
      attribute :config, StructWithMixedDefaults, public?: true
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

    test "struct DSL handles empty list defaults" do
      assert {:ok, struct} = StructWithListDefault.new(%{id: "123"})
      assert struct.id == "123"
      assert struct.tags == []
    end

    test "struct DSL handles empty map defaults" do
      assert {:ok, struct} = StructWithMapDefault.new(%{id: "456"})
      assert struct.id == "456"
      assert struct.metadata == %{}
    end

    test "struct DSL handles non-empty collection defaults" do
      assert {:ok, struct} = StructWithNonEmptyDefaults.new(%{id: "789"})
      assert struct.id == "789"
      assert struct.tags == ["default", "tag"]
      assert struct.config == %{key: "value"}
    end

    test "struct DSL handles mixed scalar and collection defaults" do
      assert {:ok, struct} = StructWithMixedDefaults.new(%{})
      assert struct.name == "John"
      assert struct.age == 30
      assert struct.active == true
      assert struct.tags == []
      assert struct.metadata == %{}
      assert struct.options == [:option1, :option2]
    end

    test "struct DSL allows overriding collection defaults" do
      assert {:ok, struct} =
               StructWithListDefault.new(%{
                 id: "custom",
                 tags: ["custom", "tags"]
               })

      assert struct.tags == ["custom", "tags"]

      assert {:ok, struct2} =
               StructWithMapDefault.new(%{
                 id: "custom2",
                 metadata: %{custom: "data"}
               })

      assert struct2.metadata == %{custom: "data"}
    end

    test "struct DSL handles nested collection defaults" do
      assert {:ok, struct} = StructWithNestedCollections.new(%{id: "nested-test"})
      assert struct.id == "nested-test"
      assert struct.nested_list == [[]]
      assert struct.list_of_maps == [%{}]
      assert struct.map_with_defaults == %{nested: %{key: "value"}}
    end

    test "struct DSL allows overriding nested collection defaults" do
      assert {:ok, struct} =
               StructWithNestedCollections.new(%{
                 id: "custom-nested",
                 nested_list: [["a", "b"], ["c"]],
                 list_of_maps: [%{a: 1}, %{b: 2}],
                 map_with_defaults: %{custom: "map"}
               })

      assert struct.nested_list == [["a", "b"], ["c"]]
      assert struct.list_of_maps == [%{a: 1}, %{b: 2}]
      assert struct.map_with_defaults == %{custom: "map"}
    end

    test "struct DSL handles array of TypedStructs with default value" do
      data = %{
        "points_balance" => 150.0,
        "rewards" => [
          %{"name" => "Free Coffee"},
          %{"name" => "10% Discount"}
        ]
      }

      assert {:ok, balance} = Balance.new(data)
      assert balance.points_balance == 150.0
      assert length(balance.rewards) == 2
      assert %Reward{name: "Free Coffee"} = Enum.at(balance.rewards, 0)
      assert %Reward{name: "10% Discount"} = Enum.at(balance.rewards, 1)
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

    test "it handles TypedStruct with collection defaults as resource attribute" do
      changeset =
        ResourceWithCollectionStruct
        |> Ash.Changeset.for_create(:create, %{})

      assert changeset.valid?

      changeset2 =
        ResourceWithCollectionStruct
        |> Ash.Changeset.for_create(:create, %{
          config: %{}
        })

      assert changeset2.valid?
      assert changeset2.attributes.config.name == "John"
      assert changeset2.attributes.config.tags == []
      assert changeset2.attributes.config.metadata == %{}

      changeset3 =
        ResourceWithCollectionStruct
        |> Ash.Changeset.for_create(:create, %{
          config: %{
            name: "Custom",
            tags: ["tag1", "tag2"],
            metadata: %{key: "value"}
          }
        })

      assert changeset3.valid?
      assert changeset3.attributes.config.tags == ["tag1", "tag2"]
      assert changeset3.attributes.config.metadata == %{key: "value"}
    end
  end

  describe "typed struct introspection" do
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
