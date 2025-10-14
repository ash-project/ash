# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Resource.UnrelatedExistsTest do
  @moduledoc false
  use ExUnit.Case, async: true

  require Ash.Query
  import Ash.Expr
  alias Ash.Test.Domain, as: Domain

  defmodule Profile do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string, public?: true
      attribute :age, :integer, public?: true
      attribute :bio, :string, public?: true
      attribute :active, :boolean, default: true, public?: true
      attribute :owner_id, :uuid, public?: true
      attribute :department, :string, public?: true
    end

    actions do
      defaults [:read, :destroy, create: :*, update: :*]
    end

    policies do
      policy action_type([:create, :update, :destroy]) do
        authorize_if always()
      end

      policy action_type(:read) do
        authorize_if actor_attribute_equals(:role, :admin)
        authorize_if expr(owner_id == ^actor(:id))
      end
    end
  end

  defmodule Report do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, public?: true
      attribute :author_name, :string, public?: true
      attribute :score, :integer, public?: true
      attribute :status, :atom, default: :draft, public?: true
      attribute :published, :boolean, default: false, public?: true

      attribute :inserted_at, :utc_datetime,
        default: &DateTime.utc_now/0,
        allow_nil?: false,
        public?: true
    end

    actions do
      defaults [:read, :destroy, update: :*]

      create :create do
        primary? true
        accept [:title, :author_name, :score, :status, :published, :inserted_at]
      end
    end
  end

  defmodule User do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string, public?: true
      attribute :age, :integer, public?: true
      attribute :email, :string, public?: true
      attribute :role, :atom, default: :user, public?: true
    end

    actions do
      defaults [:read, :destroy, create: :*, update: :*]
    end
  end

  describe "basic unrelated exists expressions" do
    setup do
      admin = Ash.create!(User, %{name: "Admin", email: "admin@example.com", role: :admin})
      user1 = Ash.create!(User, %{name: "John", email: "john@example.com", age: 30})
      user2 = Ash.create!(User, %{name: "Jane", email: "jane@example.com", age: 25})
      user3 = Ash.create!(User, %{name: "Bob", email: "bob@example.com", age: 35})

      Ash.create!(Profile, %{name: "John", age: 30, active: true}, authorize?: false)
      Ash.create!(Profile, %{name: "Jane", age: 25, active: true}, authorize?: false)
      Ash.create!(Profile, %{name: "Alice", age: 28, active: false}, authorize?: false)

      Ash.create!(Report, %{
        title: "John's Report",
        author_name: "John",
        score: 85,
        published: true
      })

      Ash.create!(Report, %{
        title: "Jane's Report",
        author_name: "Jane",
        score: 92,
        published: true
      })

      %{admin: admin, user1: user1, user2: user2, user3: user3}
    end

    test "can use unrelated exists in filter", %{user1: user1, user2: user2} do
      users =
        User
        |> Ash.Query.filter(expr(exists(Profile, name == parent(name))))
        |> Ash.read!(authorize?: false)

      user_ids = Enum.map(users, & &1.id) |> Enum.sort()
      expected_ids = [user1.id, user2.id] |> Enum.sort()

      assert user_ids == expected_ids
    end

    test "can use unrelated exists with complex conditions", %{user1: user1, user2: user2} do
      users =
        User
        |> Ash.Query.filter(expr(exists(Profile, name == parent(name) and age == parent(age))))
        |> Ash.read!(authorize?: false)

      user_ids = Enum.map(users, & &1.id) |> Enum.sort()
      expected_ids = [user1.id, user2.id] |> Enum.sort()

      assert user_ids == expected_ids
    end

    test "can use unrelated exists with no parent reference", %{admin: admin} do
      users =
        User
        |> Ash.Query.filter(expr(exists(Profile, active == true)))
        |> Ash.read!(actor: admin, authorize?: false)

      assert length(users) == 4
    end

    test "can combine unrelated exists with other filters", %{user1: user1} do
      users =
        User
        |> Ash.Query.filter(expr(name == "John" and exists(Profile, name == parent(name))))
        |> Ash.read!(authorize?: false)

      assert length(users) == 1
      assert hd(users).id == user1.id
    end

    test "can use multiple unrelated exists in same filter", %{user1: user1, user2: user2} do
      users =
        User
        |> Ash.Query.filter(
          expr(
            exists(Profile, name == parent(name)) and
              exists(Report, author_name == parent(name))
          )
        )
        |> Ash.read!(authorize?: false)

      user_ids = Enum.map(users, & &1.id) |> Enum.sort()
      expected_ids = [user1.id, user2.id] |> Enum.sort()

      assert user_ids == expected_ids
    end

    test "can negate unrelated exists", %{user3: user3, admin: admin} do
      users =
        User
        |> Ash.Query.filter(expr(not exists(Profile, name == parent(name))))
        |> Ash.read!(authorize?: false)

      assert length(users) == 2
      user_ids = Enum.map(users, & &1.id) |> Enum.sort()
      expected_ids = [user3.id, admin.id] |> Enum.sort()
      assert user_ids == expected_ids
    end

    test "unrelated exists with OR conditions" do
      users =
        User
        |> Ash.Query.filter(expr(exists(Profile, name == parent(name) or age > 27)))
        |> Ash.read!(authorize?: false)

      assert length(users) == 4
    end
  end

  describe "unrelated exists with filter_input" do
    setup do
      admin = Ash.create!(User, %{name: "Admin", email: "admin@example.com", role: :admin})
      user1 = Ash.create!(User, %{name: "User1", email: "user1@example.com", role: :user})
      user2 = Ash.create!(User, %{name: "User2", email: "user2@example.com", role: :user})

      Ash.create!(Profile, %{
        name: "Admin",
        age: 40,
        owner_id: admin.id,
        department: "Management"
      })

      Ash.create!(Profile, %{
        name: "User1",
        age: 25,
        owner_id: user1.id,
        department: "Engineering"
      })

      Ash.create!(Profile, %{
        name: "User2",
        age: 30,
        owner_id: user2.id,
        department: "Marketing"
      })

      Ash.create!(Profile, %{
        name: "User1",
        age: 26,
        owner_id: user1.id,
        department: "Engineering"
      })

      %{admin: admin, user1: user1, user2: user2}
    end

    test "filter_input with unrelated exists respects authorization", %{
      admin: admin,
      user1: user1,
      user2: user2
    } do
      users =
        User
        |> Ash.Query.filter_input(expr(exists(Profile, name == parent(name))))
        |> Ash.read!(actor: user1, authorize?: true)

      assert length(users) == 1
      assert hd(users).id == user1.id

      admin_users =
        User
        |> Ash.Query.filter_input(expr(exists(Profile, name == parent(name))))
        |> Ash.read!(actor: admin, authorize?: true)

      user_ids = Enum.map(admin_users, & &1.id) |> Enum.sort()
      expected_ids = [admin.id, user1.id, user2.id] |> Enum.sort()
      assert user_ids == expected_ids
    end

    test "filter_input with complex unrelated exists respects authorization", %{
      user1: user1,
      user2: user2
    } do
      users =
        User
        |> Ash.Query.filter_input(
          expr(exists(Profile, name == parent(name) and department == "Engineering"))
        )
        |> Ash.read!(actor: user1, authorize?: true)

      assert length(users) == 1
      assert hd(users).id == user1.id

      users2 =
        User
        |> Ash.Query.filter_input(
          expr(exists(Profile, name == parent(name) and department == "Engineering"))
        )
        |> Ash.read!(actor: user2, authorize?: true)

      assert users2 == []
    end

    test "filter_input applies primary read action authorization", %{user1: user1, user2: user2} do
      user1_profiles =
        Profile
        |> Ash.Query.filter(expr(name == "User2"))
        |> Ash.read!(actor: user1, authorize?: true)

      assert Enum.empty?(user1_profiles)

      user2_profiles =
        Profile
        |> Ash.Query.filter(expr(name == "User2"))
        |> Ash.read!(actor: user2, authorize?: true)

      assert length(user2_profiles) == 1

      users_for_user1 =
        User
        |> Ash.Query.filter_input(expr(exists(Profile, name == "User2")))
        |> Ash.read!(actor: user1, authorize?: true)

      assert Enum.empty?(users_for_user1)

      users_for_user2 =
        User
        |> Ash.Query.filter_input(expr(exists(Profile, name == "User2")))
        |> Ash.read!(actor: user2, authorize?: true)

      assert length(users_for_user2) == 3
    end

    test "nested unrelated exists with authorization", %{user1: user1} do
      Ash.create!(Report, %{
        title: "Report about User1",
        author_name: "User1",
        score: 100,
        published: true
      })

      user1_id = user1.id

      users =
        User
        |> Ash.Query.filter_input(
          expr(
            exists(
              Report,
              published == true and
                exists(Profile, name == parent(author_name) and owner_id == ^user1_id)
            )
          )
        )
        |> Ash.read!(actor: user1, authorize?: true)

      assert length(users) == 3
    end
  end

  describe "unrelated exists in calculations" do
    setup do
      user = Ash.create!(User, %{name: "Alice", email: "alice@example.com"})
      Ash.create!(Profile, %{name: "Alice", age: 25, active: true})

      Ash.create!(Report, %{
        title: "Alice's Research",
        author_name: "Alice",
        score: 95,
        published: true
      })

      %{user: user}
    end

    test "can use unrelated exists in inline calculations", %{user: user} do
      defmodule UserWithCalcs do
        use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

        ets do
          private? true
        end

        attributes do
          uuid_primary_key :id
          attribute :name, :string, public?: true
          attribute :email, :string, public?: true
        end

        calculations do
          calculate :has_profile,
                    :boolean,
                    expr(
                      exists(Ash.Test.Resource.UnrelatedExistsTest.Profile, name == parent(name))
                    ) do
            public? true
          end

          calculate :has_published_report,
                    :boolean,
                    expr(
                      exists(
                        Ash.Test.Resource.UnrelatedExistsTest.Report,
                        author_name == parent(name) and published == true
                      )
                    ) do
            public? true
          end

          calculate :profile_and_report_status,
                    :map,
                    expr(%{
                      has_profile:
                        exists(
                          Ash.Test.Resource.UnrelatedExistsTest.Profile,
                          name == parent(name)
                        ),
                      has_report:
                        exists(
                          Ash.Test.Resource.UnrelatedExistsTest.Report,
                          author_name == parent(name)
                        )
                    }) do
            public? true
          end
        end

        actions do
          defaults [:read, :destroy, create: :*, update: :*]
        end
      end

      test_user =
        Ash.create!(UserWithCalcs, %{
          name: "Alice",
          email: "alice@example.com"
        })

      loaded_user =
        UserWithCalcs
        |> Ash.Query.filter(id == ^test_user.id)
        |> Ash.Query.load([:has_profile, :has_published_report, :profile_and_report_status])
        |> Ash.read_one!()

      assert loaded_user.has_profile == true
      assert loaded_user.has_published_report == true

      assert loaded_user.profile_and_report_status == %{
               has_profile: true,
               has_report: true
             }
    end
  end

  describe "edge cases and error handling" do
    test "unrelated exists with empty result sets" do
      users =
        User
        |> Ash.Query.filter(expr(name == "NonExistent" and exists(Profile, name == parent(name))))
        |> Ash.read!()

      assert users == []
    end

    test "unrelated exists when no matching records exist" do
      user = Ash.create!(User, %{name: "Unique", email: "unique@example.com"})

      users =
        User
        |> Ash.Query.filter(expr(id == ^user.id and exists(Profile, name == parent(name))))
        |> Ash.read!()

      assert users == []
    end

    test "combining related and unrelated exists" do
      users =
        User
        |> Ash.Query.filter(
          expr(
            exists(Profile, active == true) and
              exists(Report, published == true)
          )
        )
        |> Ash.read!()

      assert is_list(users)
    end

    test "nested unrelated exists expressions" do
      expr_ast =
        expr(
          exists(
            Profile,
            active == true and
              exists(Report, author_name == parent(name))
          )
        )

      assert %Ash.Query.Exists{related?: false} = expr_ast

      assert %Ash.Query.BooleanExpression{
               right: %Ash.Query.Exists{
                 related?: false,
                 resource: Ash.Test.Resource.UnrelatedExistsTest.Report
               }
             } = expr_ast.expr
    end

    test "unrelated exists with aggregate comparisons" do
      users =
        User
        |> Ash.Query.filter(expr(exists(Profile, age > parent(age))))
        |> Ash.read!()

      assert is_list(users)
    end
  end

  describe "data layer capability checking" do
    test "ETS data layer should support unrelated exists" do
      # This will fail until we implement the capability
      assert Ash.DataLayer.data_layer_can?(User, {:exists, :unrelated}) == true
    end
  end
end
