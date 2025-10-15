# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.DataLayer.MnesiaTest do
  use ExUnit.Case, async: false

  require IEx
  alias Ash.DataLayer.Mnesia, as: MnesiaDataLayer
  alias Ash.Test.Domain, as: Domain

  @default_test_table :test_users

  setup_all do
    # Start Mnesia for the entire test suite
    :mnesia.create_schema([node()])
    :mnesia.start()
    :ok
  end

  setup do
    # Clean up any existing tables before each test
    try do
      :mnesia.delete_table(@default_test_table)
    catch
      :exit, {:aborted, {:no_exists, _}} -> :ok
    end

    # Create fresh table for each test
    :mnesia.create_table(@default_test_table, attributes: [:id, :val])
    :ok
  end

  defmodule MnesiaTestUser do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Mnesia

    mnesia do
      table :test_users
    end

    actions do
      defaults [:read, :destroy, create: :*]

      update :update do
        accept :*
        require_atomic? false
      end
    end

    identities do
      identity :unique_name, [:name], pre_check_with: Domain
    end

    attributes do
      uuid_primary_key :id, writable?: true
      attribute :name, :string, public?: true
      attribute :age, :integer, public?: true
      attribute :title, :string, public?: true
      attribute :roles, {:array, :atom}, public?: true
    end
  end

  describe "create/2" do
    test "it creates a user" do
      resource = MnesiaTestUser
      user = %{name: "John", age: 30, title: "Developer", roles: [:admin, :user]}

      assert {:ok, created_user} =
               MnesiaDataLayer.create(resource, Ash.Changeset.for_create(resource, :create, user))

      assert %MnesiaTestUser{
               id: _,
               name: "John",
               age: 30,
               title: "Developer",
               roles: [:admin, :user]
             } = created_user
    end
  end

  describe "bulk_create/3" do
    test "can? bulk_create" do
      assert MnesiaDataLayer.can?(:test_resource, :bulk_create)
    end

    test "bulk_create/3 without returning records" do
      resp =
        [
          %{name: "John", age: 30, title: "Developer", roles: [:admin, :user]},
          %{name: "Jane", age: 25, title: "Designer", roles: [:user]},
          %{name: "Bob", age: 35, title: "Manager", roles: [:admin, :manager]}
        ]
        |> Ash.bulk_create(MnesiaTestUser, :create)

      assert %Ash.BulkResult{
               status: :success,
               errors: [],
               records: nil,
               notifications: nil,
               error_count: 0
             } = resp
    end

    test "bulk_create/3 WITH returning records" do
      resp =
        [
          %{name: "John", age: 30, title: "Developer", roles: [:admin, :user]},
          %{name: "Jane", age: 25, title: "Designer", roles: [:user]},
          %{name: "Bob", age: 35, title: "Manager", roles: [:admin, :manager]}
        ]
        |> Ash.bulk_create(MnesiaTestUser, :create, return_records?: true)

      assert %Ash.BulkResult{
               status: :success,
               errors: [],
               records: [
                 %MnesiaTestUser{
                   name: "John",
                   age: 30,
                   title: "Developer",
                   roles: [:admin, :user]
                 },
                 %MnesiaTestUser{name: "Jane", age: 25, title: "Designer", roles: [:user]},
                 %MnesiaTestUser{
                   name: "Bob",
                   age: 35,
                   title: "Manager",
                   roles: [:admin, :manager]
                 }
               ],
               notifications: nil,
               error_count: 0
             } = resp
    end

    test "bulk_create/3 with UPSERT without returning records" do
      user_a =
        %{name: "John", age: 30, title: "Developer", roles: [:admin, :user]}
        |> then(&Ash.create!(MnesiaTestUser, &1))
        |> resource_to_map()

      resp =
        [
          %{user_a | name: "Johnny", age: 31},
          %{name: "Jane", age: 25, title: "Designer", roles: [:user]},
          %{name: "Bob", age: 35, title: "Manager", roles: [:admin, :manager]}
        ]
        |> Ash.bulk_create(MnesiaTestUser, :create,
          upsert?: true,
          upsert_fields: [:name],
          return_records?: true
        )

      assert %Ash.BulkResult{
               status: :success,
               errors: [],
               records: [
                 %MnesiaTestUser{
                   name: "Johnny",
                   age: 30,
                   title: "Developer",
                   roles: [:admin, :user]
                 },
                 %MnesiaTestUser{name: "Jane", age: 25, title: "Designer", roles: [:user]},
                 %MnesiaTestUser{
                   name: "Bob",
                   age: 35,
                   title: "Manager",
                   roles: [:admin, :manager]
                 }
               ],
               notifications: nil,
               error_count: 0
             } = resp
    end
  end

  defp resource_to_map(resource) do
    Ash.Resource.Info.public_attributes(resource)
    |> Enum.map(& &1.name)
    |> Map.new(fn name -> {name, Map.get(resource, name)} end)
  end
end
