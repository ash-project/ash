# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Resource.UnrelatedAggregatesTest do
  @moduledoc false
  use ExUnit.Case, async: true

  require Ash.Query
  import Ash.Expr
  alias Ash.Test.Domain, as: Domain
  alias Ash.Test.Resource.UnrelatedAggregatesTest.{Profile, Report, SecureProfile, User}

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
    end

    actions do
      defaults [:read, :destroy, create: :*, update: :*]
    end

    policies do
      policy action_type([:create, :update, :destroy]) do
        authorize_if always()
      end

      policy action_type(:read) do
        authorize_if always()
      end
    end
  end

  defmodule SecureProfile do
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

      attribute :inserted_at, :utc_datetime,
        public?: true,
        default: &DateTime.utc_now/0,
        allow_nil?: false
    end

    actions do
      defaults [:read, :destroy, update: :*]

      create :create do
        primary? true
        accept [:title, :author_name, :score, :inserted_at]
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
      attribute :role, :atom, public?: true, default: :user
    end

    aggregates do
      count :matching_name_profiles_count, Profile do
        filter expr(name == parent(name))
        public? true
      end

      count :total_active_profiles, Profile do
        filter expr(active == true)
        public? true
      end

      first :latest_authored_report, Report, :title do
        filter expr(author_name == parent(name))
        sort inserted_at: :desc
        public? true
      end

      sum :total_report_score, Report, :score do
        filter expr(author_name == parent(name))
        public? true
      end

      exists :has_matching_name_profile, Profile do
        filter expr(name == parent(name))
        public? true
      end

      list :matching_profile_names, Profile, :name do
        filter expr(name == parent(name))
        public? true
      end

      max :max_age_same_name, Profile, :age do
        filter expr(name == parent(name))
        public? true
      end

      min :min_age_same_name, Profile, :age do
        filter expr(name == parent(name))
        public? true
      end

      avg :avg_age_same_name, Profile, :age do
        filter expr(name == parent(name))
        public? true
      end

      count :secure_profile_count, SecureProfile do
        filter expr(name == parent(name))
        public? true
      end
    end

    calculations do
      calculate :matching_profiles_summary,
                :string,
                expr("Found " <> type(matching_name_profiles_count, :string) <> " profiles") do
        public? true
      end

      calculate :inline_profile_count,
                :integer,
                expr(count(Profile, filter: expr(name == parent(name)))) do
        public? true
      end

      calculate :inline_latest_report_title,
                :string,
                expr(
                  first(Report,
                    field: :title,
                    query: [
                      filter: expr(author_name == parent(name)),
                      sort: [inserted_at: :desc]
                    ]
                  )
                ) do
        public? true
      end

      calculate :inline_total_score,
                :integer,
                expr(
                  sum(Report,
                    field: :score,
                    query: [
                      filter: expr(author_name == parent(name))
                    ]
                  )
                ) do
        public? true
      end

      calculate :complex_calculation,
                :map,
                expr(%{
                  profile_count: count(Profile, filter: expr(name == parent(name))),
                  latest_report:
                    first(Report,
                      field: :title,
                      query: [
                        filter: expr(author_name == parent(name)),
                        sort: [inserted_at: :desc]
                      ]
                    ),
                  total_score:
                    sum(Report,
                      field: :score,
                      query: [
                        filter: expr(author_name == parent(name))
                      ]
                    )
                }) do
        public? true
      end
    end

    actions do
      defaults [:read, :destroy, create: :*, update: :*]
    end
  end

  describe "basic unrelated aggregate definitions" do
    test "aggregates are properly defined with related?: false" do
      aggregates = Ash.Resource.Info.aggregates(User)

      count_agg = Enum.find(aggregates, &(&1.name == :matching_name_profiles_count))
      assert count_agg
      assert count_agg.related? == false
      assert count_agg.resource == Profile
      assert count_agg.kind == :count
      assert count_agg.relationship_path == []

      first_agg = Enum.find(aggregates, &(&1.name == :latest_authored_report))
      assert first_agg
      assert first_agg.related? == false
      assert first_agg.resource == Report
      assert first_agg.kind == :first
      assert first_agg.field == :title

      sum_agg = Enum.find(aggregates, &(&1.name == :total_report_score))
      assert sum_agg
      assert sum_agg.related? == false
      assert sum_agg.resource == Report
      assert sum_agg.kind == :sum
      assert sum_agg.field == :score
    end

    test "unrelated aggregates support all aggregate kinds" do
      aggregates = Ash.Resource.Info.aggregates(User)
      aggregate_names = Enum.map(aggregates, & &1.name)

      # Verify all kinds are supported
      # count
      assert :matching_name_profiles_count in aggregate_names
      # first
      assert :latest_authored_report in aggregate_names
      # sum
      assert :total_report_score in aggregate_names
      # exists
      assert :has_matching_name_profile in aggregate_names
      # list
      assert :matching_profile_names in aggregate_names
      # max
      assert :max_age_same_name in aggregate_names
      # min
      assert :min_age_same_name in aggregate_names
      # avg
      assert :avg_age_same_name in aggregate_names
    end

    test "can define aggregates without parent filters" do
      aggregates = Ash.Resource.Info.aggregates(User)
      total_active_agg = Enum.find(aggregates, &(&1.name == :total_active_profiles))

      assert total_active_agg
      assert total_active_agg.related? == false
      assert total_active_agg.resource == Profile
      # Should have filter but no parent() reference
    end
  end

  describe "loading unrelated aggregates" do
    setup do
      user1 = Ash.create!(User, %{name: "John", email: "john@example.com"})
      user2 = Ash.create!(User, %{name: "Jane", email: "jane@example.com"})

      Ash.create!(Profile, %{name: "John", age: 25, active: true})
      Ash.create!(Profile, %{name: "John", age: 30, active: true})
      Ash.create!(Profile, %{name: "Jane", age: 28, active: true})
      Ash.create!(Profile, %{name: "Bob", age: 35, active: false})

      base_time = ~U[2024-01-01 12:00:00Z]

      Ash.create!(Report, %{
        title: "John's First Report",
        author_name: "John",
        score: 85,
        inserted_at: base_time
      })

      Ash.create!(Report, %{
        title: "John's Latest Report",
        author_name: "John",
        score: 92,
        inserted_at: DateTime.add(base_time, 3600, :second)
      })

      Ash.create!(Report, %{
        title: "Jane's Report",
        author_name: "Jane",
        score: 78
      })

      %{user1: user1, user2: user2}
    end

    test "can load count unrelated aggregates", %{user1: user1, user2: user2} do
      users =
        User
        |> Ash.Query.load([:matching_name_profiles_count, :total_active_profiles])
        |> Ash.read!()

      john = Enum.find(users, &(&1.id == user1.id))
      jane = Enum.find(users, &(&1.id == user2.id))

      assert john.matching_name_profiles_count == 2
      assert john.total_active_profiles == 3

      assert jane.matching_name_profiles_count == 1
      assert jane.total_active_profiles == 3
    end

    test "can load first unrelated aggregates", %{user1: user1} do
      user =
        User
        |> Ash.Query.filter(id == ^user1.id)
        |> Ash.Query.load(:latest_authored_report)
        |> Ash.read_one!()

      assert user.latest_authored_report == "John's Latest Report"
    end

    test "can load sum unrelated aggregates", %{user1: user1, user2: user2} do
      users =
        User
        |> Ash.Query.load(:total_report_score)
        |> Ash.read!()

      john = Enum.find(users, &(&1.id == user1.id))
      jane = Enum.find(users, &(&1.id == user2.id))

      assert john.total_report_score == 177
      assert jane.total_report_score == 78
    end

    test "can load exists unrelated aggregates", %{user1: user1} do
      user =
        User
        |> Ash.Query.filter(id == ^user1.id)
        |> Ash.Query.load(:has_matching_name_profile)
        |> Ash.read_one!()

      assert user.has_matching_name_profile == true
    end

    test "can load list unrelated aggregates", %{user1: user1} do
      user =
        User
        |> Ash.Query.filter(id == ^user1.id)
        |> Ash.Query.load(:matching_profile_names)
        |> Ash.read_one!()

      assert length(user.matching_profile_names) == 2
      assert Enum.all?(user.matching_profile_names, &(&1 == "John"))
    end

    test "can load min/max/avg unrelated aggregates", %{user1: user1} do
      user =
        User
        |> Ash.Query.filter(id == ^user1.id)
        |> Ash.Query.load([:min_age_same_name, :max_age_same_name, :avg_age_same_name])
        |> Ash.read_one!()

      assert user.min_age_same_name == 25
      assert user.max_age_same_name == 30
      assert user.avg_age_same_name == 27.5
    end
  end

  describe "unrelated aggregates in calculations" do
    setup do
      user = Ash.create!(User, %{name: "Alice", email: "alice@example.com"})
      Ash.create!(Profile, %{name: "Alice", age: 25, active: true})

      Ash.create!(Report, %{
        title: "Alice's Research",
        author_name: "Alice",
        score: 95,
        inserted_at: ~U[2024-01-01 12:00:00Z]
      })

      %{user: user}
    end

    test "calculations using named unrelated aggregates work", %{user: user} do
      user =
        User
        |> Ash.Query.filter(id == ^user.id)
        |> Ash.Query.load(:matching_profiles_summary)
        |> Ash.read_one!()

      assert user.matching_profiles_summary == "Found 1 profiles"
    end

    test "inline unrelated aggregates in calculations work", %{user: user} do
      user =
        User
        |> Ash.Query.filter(id == ^user.id)
        |> Ash.Query.load([
          :inline_profile_count,
          :inline_latest_report_title,
          :inline_total_score
        ])
        |> Ash.read_one!()

      assert user.inline_profile_count == 1
      assert user.inline_latest_report_title == "Alice's Research"
      assert user.inline_total_score == 95
    end

    test "complex calculations with multiple inline unrelated aggregates work", %{user: user} do
      user =
        User
        |> Ash.Query.filter(id == ^user.id)
        |> Ash.Query.load(:complex_calculation)
        |> Ash.read_one!()

      assert user.complex_calculation == %{
               profile_count: 1,
               latest_report: "Alice's Research",
               total_score: 95
             }
    end
  end

  describe "data layer capability checking" do
    test "ETS data layer should support unrelated aggregates" do
      assert Ash.DataLayer.data_layer_can?(Profile, {:aggregate, :unrelated}) == true
    end
  end

  describe "authorization with unrelated aggregates" do
    test "unrelated aggregates work without relationship path authorization errors" do
      user = Ash.create!(User, %{name: "AuthTest", email: "auth@example.com"})
      Ash.create!(Profile, %{name: "AuthTest", age: 25, active: true})

      user =
        User
        |> Ash.Query.filter(id == ^user.id)
        |> Ash.Query.load(:matching_name_profiles_count)
        |> Ash.read_one!()

      assert user.matching_name_profiles_count == 1
    end

    test "unrelated aggregates in calculations don't cause authorization errors" do
      user = Ash.create!(User, %{name: "CalcAuth", email: "calcauth@example.com"})
      Ash.create!(Profile, %{name: "CalcAuth", age: 30, active: true})

      user =
        User
        |> Ash.Query.filter(id == ^user.id)
        |> Ash.Query.load(:matching_profiles_summary)
        |> Ash.read_one!()

      assert user.matching_profiles_summary == "Found 1 profiles"
    end

    test "multiple unrelated aggregates can be loaded together without authorization issues" do
      user = Ash.create!(User, %{name: "MultiAuth", email: "multi@example.com"})
      Ash.create!(Profile, %{name: "MultiAuth", age: 28, active: true})

      Ash.create!(Report, %{
        title: "MultiAuth Report",
        author_name: "MultiAuth",
        score: 88,
        inserted_at: ~U[2024-01-01 15:00:00Z]
      })

      user =
        User
        |> Ash.Query.filter(id == ^user.id)
        |> Ash.Query.load([
          :matching_name_profiles_count,
          :total_active_profiles,
          :latest_authored_report,
          :total_report_score
        ])
        |> Ash.read_one!()

      assert user.matching_name_profiles_count == 1
      assert user.total_active_profiles >= 1
      assert user.latest_authored_report == "MultiAuth Report"
      assert user.total_report_score == 88
    end

    test "unrelated aggregates respect target resource authorization policies" do
      admin_user = Ash.create!(User, %{name: "Admin", email: "admin@test.com", role: :admin})
      regular_user1 = Ash.create!(User, %{name: "User1", email: "user1@test.com", role: :user})
      regular_user2 = Ash.create!(User, %{name: "User1", email: "user2@test.com", role: :user})

      Ash.create!(SecureProfile, %{
        name: "User1",
        age: 25,
        active: true,
        owner_id: regular_user1.id,
        department: "Engineering"
      })

      Ash.create!(SecureProfile, %{
        name: "User1",
        age: 30,
        active: true,
        owner_id: regular_user2.id,
        department: "Marketing"
      })

      Ash.create!(SecureProfile, %{
        name: "Admin",
        age: 35,
        active: true,
        owner_id: admin_user.id,
        department: "Management"
      })

      user1_result =
        User
        |> Ash.Query.filter(id == ^regular_user1.id)
        |> Ash.Query.load(:secure_profile_count)
        |> Ash.read_one!(actor: regular_user1, authorize?: true)

      assert user1_result.secure_profile_count == 1

      user2_result =
        User
        |> Ash.Query.filter(id == ^regular_user2.id)
        |> Ash.Query.load(:secure_profile_count)
        |> Ash.read_one!(actor: regular_user2, authorize?: true)

      assert user2_result.secure_profile_count == 1

      admin_as_user1 =
        User
        |> Ash.Query.filter(id == ^regular_user1.id)
        |> Ash.Query.load(:secure_profile_count)
        |> Ash.read_one!(actor: admin_user, authorize?: true)

      assert admin_as_user1.secure_profile_count == 2

      admin_result =
        User
        |> Ash.Query.filter(id == ^admin_user.id)
        |> Ash.Query.load(:secure_profile_count)
        |> Ash.read_one!(actor: admin_user, authorize?: true)

      assert admin_result.secure_profile_count == 1
    end

    test "unrelated aggregates in calculations respect authorization" do
      user = Ash.create!(User, %{name: "CalcTest", email: "calc@test.com", role: :user})
      other_user = Ash.create!(User, %{name: "CalcTest", email: "other@test.com", role: :user})

      Ash.create!(SecureProfile, %{
        name: "CalcTest",
        age: 25,
        active: true,
        owner_id: user.id,
        department: "Engineering"
      })

      Ash.create!(SecureProfile, %{
        name: "CalcTest",
        age: 30,
        active: true,
        owner_id: other_user.id,
        department: "Marketing"
      })

      defmodule UserWithCalc do
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
          attribute :email, :string, public?: true
          attribute :role, :atom, public?: true, default: :user
        end

        aggregates do
          count :secure_profile_count, SecureProfile do
            filter expr(name == parent(name))
            public? true
          end
        end

        calculations do
          calculate :profile_summary,
                    :string,
                    expr("Found " <> type(secure_profile_count, :string) <> " secure profiles") do
            public? true
          end
        end

        actions do
          defaults [:read, :destroy, create: :*, update: :*]
        end

        policies do
          policy action_type([:create, :update, :destroy, :read]) do
            authorize_if always()
          end
        end
      end

      test_user =
        Ash.create!(UserWithCalc, %{
          name: "CalcTest",
          email: "calc@test.com",
          role: :user
        })

      result =
        UserWithCalc
        |> Ash.Query.filter(id == ^test_user.id)
        |> Ash.Query.load(:profile_summary)
        |> Ash.read_one!(actor: %{id: user.id, role: :user}, authorize?: true)

      assert result.profile_summary == "Found 1 secure profiles"
    end
  end

  describe "edge cases" do
    test "unrelated aggregates work with empty result sets" do
      users =
        User
        |> Ash.Query.filter(name == "NonExistent")
        |> Ash.Query.load(:matching_name_profiles_count)
        |> Ash.read!()

      assert users == []
    end

    test "unrelated aggregates work with filters that return no results" do
      user = Ash.create!(User, %{name: "Unique", email: "unique@example.com"})

      loaded_user =
        User
        |> Ash.Query.filter(id == ^user.id)
        |> Ash.Query.load(:matching_name_profiles_count)
        |> Ash.read_one!()

      assert loaded_user.matching_name_profiles_count == 0
    end

    test "unrelated aggregates work with complex filter expressions" do
      user = Ash.create!(User, %{name: "ComplexTest", age: 25, email: "complex@example.com"})

      Ash.create!(Profile, %{name: "ComplexTest", age: 25, bio: "Bio contains ComplexTest"})
      Ash.create!(Profile, %{name: "ComplexTest", age: 30, bio: "Different bio"})
      Ash.create!(Profile, %{name: "Other", age: 25, bio: "ComplexTest mentioned"})

      loaded_user =
        User
        |> Ash.Query.filter(id == ^user.id)
        |> Ash.Query.aggregate(:same_name_and_age, :count, Profile,
          query: [filter: expr(name == parent(name) and age == parent(age))]
        )
        |> Ash.read_one!(domain: Domain)

      assert loaded_user.aggregates.same_name_and_age == 1

      loaded_user =
        User
        |> Ash.Query.filter(id == ^user.id)
        |> Ash.Query.aggregate(:name_or_bio_match, :count, Profile,
          query: [filter: expr(name == parent(name) or contains(bio, parent(name)))]
        )
        |> Ash.read_one!(domain: Domain)

      assert loaded_user.aggregates.name_or_bio_match == 3

      loaded_user =
        User
        |> Ash.Query.filter(id == ^user.id)
        |> Ash.Query.aggregate(:older_profiles, :count, Profile,
          query: [filter: expr(name == parent(name) and age > parent(age))]
        )
        |> Ash.read_one!(domain: Domain)

      assert loaded_user.aggregates.older_profiles == 1
    end

    test "parent() works with nested conditional expressions" do
      user = Ash.create!(User, %{name: "NestedTest", age: 30, email: "nested@example.com"})

      Ash.create!(Profile, %{name: "NestedTest", age: 25, bio: "Young"})
      Ash.create!(Profile, %{name: "NestedTest", age: 35, bio: "Old"})
      Ash.create!(Profile, %{name: "Other", age: 30, bio: "Same age"})

      loaded_user =
        User
        |> Ash.Query.filter(id == ^user.id)
        |> Ash.Query.aggregate(:complex_condition, :count, Profile,
          query: [filter: expr(name == parent(name) and (age < parent(age) or age > parent(age)))]
        )
        |> Ash.read_one!(domain: Domain)

      assert loaded_user.aggregates.complex_condition == 2
    end

    test "parent() works with string functions" do
      user = Ash.create!(User, %{name: "StringTest", email: "string@example.com"})

      Ash.create!(Profile, %{name: "StringTest", bio: "StringTest is mentioned here"})
      Ash.create!(Profile, %{name: "DifferentName", bio: "StringTest appears in bio"})
      Ash.create!(Profile, %{name: "StringTest", bio: "No mention"})

      loaded_user =
        User
        |> Ash.Query.filter(id == ^user.id)
        |> Ash.Query.aggregate(:bio_mentions_name, :count, Profile,
          query: [filter: expr(contains(bio, parent(name)))]
        )
        |> Ash.read_one!(domain: Domain)

      assert loaded_user.aggregates.bio_mentions_name == 2
    end
  end
end
