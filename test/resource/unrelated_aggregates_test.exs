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
      # Allow unrestricted access for most tests, but we'll create a SecureProfile for auth tests
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
      # Allow creation/updates for testing setup
      policy action_type([:create, :update, :destroy]) do
        authorize_if always()
      end

      # Only allow users to see their own profiles, or admins to see all
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

    # Test basic unrelated aggregates
    aggregates do
      # Count of profiles with matching name
      count :matching_name_profiles_count, Profile do
        filter expr(name == parent(name))
        public? true
      end

      # Count of all active profiles (no parent filter)
      count :total_active_profiles, Profile do
        filter expr(active == true)
        public? true
      end

      # First report with matching author name
      first :latest_authored_report, Report, :title do
        filter expr(author_name == parent(name))
        sort inserted_at: :desc
        public? true
      end

      # Sum of report scores for matching author
      sum :total_report_score, Report, :score do
        filter expr(author_name == parent(name))
        public? true
      end

      # Exists check for profiles with same name
      exists :has_matching_name_profile, Profile do
        filter expr(name == parent(name))
        public? true
      end

      # List of all profile names with same name (should be just one usually)
      list :matching_profile_names, Profile, :name do
        filter expr(name == parent(name))
        public? true
      end

      # Max age of profiles with same name
      max :max_age_same_name, Profile, :age do
        filter expr(name == parent(name))
        public? true
      end

      # Min age of profiles with same name
      min :min_age_same_name, Profile, :age do
        filter expr(name == parent(name))
        public? true
      end

      # Average age of profiles with same name
      avg :avg_age_same_name, Profile, :age do
        filter expr(name == parent(name))
        public? true
      end

      # Secure aggregate - should respect authorization policies
      count :secure_profile_count, SecureProfile do
        filter expr(name == parent(name))
        public? true
      end
    end

    # Test unrelated aggregates in calculations
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
      # Create test data
      {:ok, user1} = Ash.create(User, %{name: "John", email: "john@example.com"})
      {:ok, user2} = Ash.create(User, %{name: "Jane", email: "jane@example.com"})

      {:ok, _profile1} = Ash.create(Profile, %{name: "John", age: 25, active: true})
      {:ok, _profile2} = Ash.create(Profile, %{name: "John", age: 30, active: true})
      {:ok, _profile3} = Ash.create(Profile, %{name: "Jane", age: 28, active: true})
      {:ok, _profile4} = Ash.create(Profile, %{name: "Bob", age: 35, active: false})

      base_time = ~U[2024-01-01 12:00:00Z]

      {:ok, _report1} =
        Ash.create(Report, %{
          title: "John's First Report",
          author_name: "John",
          score: 85,
          inserted_at: base_time
        })

      {:ok, _report2} =
        Ash.create(Report, %{
          title: "John's Latest Report",
          author_name: "John",
          score: 92,
          inserted_at: DateTime.add(base_time, 3600, :second)
        })

      {:ok, _report3} =
        Ash.create(Report, %{
          title: "Jane's Report",
          author_name: "Jane",
          score: 78
        })

      %{user1: user1, user2: user2}
    end

    test "can load count unrelated aggregates", %{user1: user1, user2: user2} do
      # Load users with aggregates
      users =
        User
        |> Ash.Query.load([:matching_name_profiles_count, :total_active_profiles])
        |> Ash.read!()

      john = Enum.find(users, &(&1.id == user1.id))
      jane = Enum.find(users, &(&1.id == user2.id))

      # John should have 2 matching profiles
      assert john.matching_name_profiles_count == 2
      # Both should see 3 total active profiles (John x2, Jane x1)
      assert john.total_active_profiles == 3

      # Jane should have 1 matching profile
      assert jane.matching_name_profiles_count == 1
      assert jane.total_active_profiles == 3
    end

    test "can load first unrelated aggregates", %{user1: user1} do
      user =
        User
        |> Ash.Query.filter(id == ^user1.id)
        |> Ash.Query.load(:latest_authored_report)
        |> Ash.read_one!()

      # Should get the latest report title
      assert user.latest_authored_report == "John's Latest Report"
    end

    test "can load sum unrelated aggregates", %{user1: user1, user2: user2} do
      users =
        User
        |> Ash.Query.load(:total_report_score)
        |> Ash.read!()

      john = Enum.find(users, &(&1.id == user1.id))
      jane = Enum.find(users, &(&1.id == user2.id))

      # John's total score: 85 + 92 = 177
      assert john.total_report_score == 177
      # Jane's total score: 78
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

      # Should have two "John" entries
      assert length(user.matching_profile_names) == 2
      assert Enum.all?(user.matching_profile_names, &(&1 == "John"))
    end

    test "can load min/max/avg unrelated aggregates", %{user1: user1} do
      user =
        User
        |> Ash.Query.filter(id == ^user1.id)
        |> Ash.Query.load([:min_age_same_name, :max_age_same_name, :avg_age_same_name])
        |> Ash.read_one!()

      # John profiles have ages 25 and 30
      assert user.min_age_same_name == 25
      assert user.max_age_same_name == 30
      assert user.avg_age_same_name == 27.5
    end
  end

  describe "unrelated aggregates in calculations" do
    setup do
      {:ok, user} = Ash.create(User, %{name: "Alice", email: "alice@example.com"})
      {:ok, _profile} = Ash.create(Profile, %{name: "Alice", age: 25, active: true})

      {:ok, _report} =
        Ash.create(Report, %{
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
      # This will fail until we implement the capability
      assert Ash.DataLayer.data_layer_can?(Profile, {:aggregate, :unrelated}) == true
    end

    test "error when data layer doesn't support unrelated aggregates" do
      # Test with a mock data layer that doesn't support unrelated aggregates
      # This will be relevant when we add the capability checking
    end
  end

  describe "authorization with unrelated aggregates" do
    # These tests verify that authorization works properly for unrelated aggregates
    # The main concern is that unrelated aggregates don't have relationship paths,
    # so the authorization logic must handle this correctly

    test "unrelated aggregates work without relationship path authorization errors" do
      # This test verifies that unrelated aggregates don't trigger the
      # :lists.droplast([]) error that was happening before the fix
      {:ok, user} = Ash.create(User, %{name: "AuthTest", email: "auth@example.com"})
      {:ok, _profile} = Ash.create(Profile, %{name: "AuthTest", age: 25, active: true})

      # This should not raise authorization errors
      user =
        User
        |> Ash.Query.filter(id == ^user.id)
        |> Ash.Query.load(:matching_name_profiles_count)
        |> Ash.read_one!()

      assert user.matching_name_profiles_count == 1
    end

    test "unrelated aggregates in calculations don't cause authorization errors" do
      # Test that the authorization logic correctly handles unrelated aggregates
      # when they're referenced in calculations
      {:ok, user} = Ash.create(User, %{name: "CalcAuth", email: "calcauth@example.com"})
      {:ok, _profile} = Ash.create(Profile, %{name: "CalcAuth", age: 30, active: true})

      # This should not raise authorization errors
      user =
        User
        |> Ash.Query.filter(id == ^user.id)
        |> Ash.Query.load(:matching_profiles_summary)
        |> Ash.read_one!()

      assert user.matching_profiles_summary == "Found 1 profiles"
    end

    test "multiple unrelated aggregates can be loaded together without authorization issues" do
      # Test loading multiple unrelated aggregates simultaneously
      {:ok, user} = Ash.create(User, %{name: "MultiAuth", email: "multi@example.com"})
      {:ok, _profile} = Ash.create(Profile, %{name: "MultiAuth", age: 28, active: true})

      {:ok, _report} =
        Ash.create(Report, %{
          title: "MultiAuth Report",
          author_name: "MultiAuth",
          score: 88,
          inserted_at: ~U[2024-01-01 15:00:00Z]
        })

      # Loading multiple unrelated aggregates should work
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
      # Could include profiles from other tests
      assert user.total_active_profiles >= 1
      assert user.latest_authored_report == "MultiAuth Report"
      assert user.total_report_score == 88
    end

    test "unrelated aggregates respect target resource authorization policies" do
      # Create users with different roles
      {:ok, admin_user} =
        Ash.create(User, %{name: "Admin", email: "admin@test.com", role: :admin})

      {:ok, regular_user1} =
        Ash.create(User, %{name: "User1", email: "user1@test.com", role: :user})

      # Same name as user1
      {:ok, regular_user2} =
        Ash.create(User, %{name: "User1", email: "user2@test.com", role: :user})

      # Create secure profiles with different owners
      {:ok, _profile1} =
        Ash.create(SecureProfile, %{
          name: "User1",
          age: 25,
          active: true,
          owner_id: regular_user1.id,
          department: "Engineering"
        })

      {:ok, _profile2} =
        Ash.create(SecureProfile, %{
          name: "User1",
          age: 30,
          active: true,
          owner_id: regular_user2.id,
          department: "Marketing"
        })

      {:ok, _profile3} =
        Ash.create(SecureProfile, %{
          name: "Admin",
          age: 35,
          active: true,
          owner_id: admin_user.id,
          department: "Management"
        })

      # Regular user1 should only see their own profile in the aggregate
      user1_result =
        User
        |> Ash.Query.filter(id == ^regular_user1.id)
        |> Ash.Query.load(:secure_profile_count)
        |> Ash.read_one!(actor: regular_user1, authorize?: true)

      # Verify that user1 only sees their own profile in the aggregate
      # This is the critical security test - aggregates must respect authorization
      # Only sees their own profile
      assert user1_result.secure_profile_count == 1

      # Regular user2 should only see their own profile in the aggregate
      user2_result =
        User
        |> Ash.Query.filter(id == ^regular_user2.id)
        |> Ash.Query.load(:secure_profile_count)
        |> Ash.read_one!(actor: regular_user2, authorize?: true)

      # Only sees their own profile
      assert user2_result.secure_profile_count == 1

      # Admin should see all profiles with matching name (both User1 profiles)
      admin_as_user1 =
        User
        # Get User1's data but as admin
        |> Ash.Query.filter(id == ^regular_user1.id)
        |> Ash.Query.load(:secure_profile_count)
        |> Ash.read_one!(actor: admin_user, authorize?: true)

      # Admin sees both User1 profiles
      assert admin_as_user1.secure_profile_count == 2

      # Admin should see their own profile when looking at themselves
      admin_result =
        User
        |> Ash.Query.filter(id == ^admin_user.id)
        |> Ash.Query.load(:secure_profile_count)
        |> Ash.read_one!(actor: admin_user, authorize?: true)

      # Admin sees their own profile
      assert admin_result.secure_profile_count == 1
    end

    test "unrelated aggregates in calculations respect authorization" do
      # Test that authorization works when aggregates are used in calculations
      {:ok, user} = Ash.create(User, %{name: "CalcTest", email: "calc@test.com", role: :user})

      {:ok, other_user} =
        Ash.create(User, %{name: "CalcTest", email: "other@test.com", role: :user})

      # Create profiles owned by different users but with same name
      {:ok, _my_profile} =
        Ash.create(SecureProfile, %{
          name: "CalcTest",
          age: 25,
          active: true,
          owner_id: user.id,
          department: "Engineering"
        })

      {:ok, _other_profile} =
        Ash.create(SecureProfile, %{
          name: "CalcTest",
          age: 30,
          active: true,
          owner_id: other_user.id,
          department: "Marketing"
        })

      # Add a calculation that uses the secure aggregate
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

      # Create user in the new resource for testing
      {:ok, test_user} =
        Ash.create(UserWithCalc, %{
          name: "CalcTest",
          email: "calc@test.com",
          role: :user
        })

      # User should only count their own profile in the calculation
      result =
        UserWithCalc
        |> Ash.Query.filter(id == ^test_user.id)
        |> Ash.Query.load(:profile_summary)
        |> Ash.read_one!(actor: %{id: user.id, role: :user}, authorize?: true)

      assert result.profile_summary == "Found 1 secure profiles"
    end
  end

  describe "error cases" do
    test "parent() function requires valid field reference" do
      # Test that parent(invalid_field) raises an error during query execution
      # This would be caught at runtime, not compile time
    end
  end

  describe "edge cases" do
    test "unrelated aggregates work with empty result sets" do
      users =
        User
        |> Ash.Query.filter(name == "NonExistent")
        |> Ash.Query.load(:matching_name_profiles_count)
        |> Ash.read!()

      # Should be empty, but aggregate should still work
      assert users == []
    end

    test "unrelated aggregates work with filters that return no results" do
      {:ok, user} = Ash.create(User, %{name: "Unique", email: "unique@example.com"})

      # No profiles with name "Unique" exist
      loaded_user =
        User
        |> Ash.Query.filter(id == ^user.id)
        |> Ash.Query.load(:matching_name_profiles_count)
        |> Ash.read_one!()

      assert loaded_user.matching_name_profiles_count == 0
    end

    test "unrelated aggregates work with complex filter expressions" do
      {:ok, user} =
        Ash.create(User, %{name: "ComplexTest", age: 25, email: "complex@example.com"})

      # Create profiles with various attributes
      {:ok, _profile1} =
        Ash.create(Profile, %{name: "ComplexTest", age: 25, bio: "Bio contains ComplexTest"})

      {:ok, _profile2} =
        Ash.create(Profile, %{name: "ComplexTest", age: 30, bio: "Different bio"})

      {:ok, _profile3} =
        Ash.create(Profile, %{name: "Other", age: 25, bio: "ComplexTest mentioned"})

      # Test parent() with boolean AND
      loaded_user =
        User
        |> Ash.Query.filter(id == ^user.id)
        |> Ash.Query.aggregate(:same_name_and_age, :count, Profile,
          query: [filter: expr(name == parent(name) and age == parent(age))]
        )
        |> Ash.read_one!(domain: Domain)

      assert loaded_user.aggregates.same_name_and_age == 1

      # Test parent() with OR conditions
      loaded_user =
        User
        |> Ash.Query.filter(id == ^user.id)
        |> Ash.Query.aggregate(:name_or_bio_match, :count, Profile,
          query: [filter: expr(name == parent(name) or contains(bio, parent(name)))]
        )
        |> Ash.read_one!(domain: Domain)

      assert loaded_user.aggregates.name_or_bio_match == 3

      # Test parent() with comparison operators
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
      {:ok, user} = Ash.create(User, %{name: "NestedTest", age: 30, email: "nested@example.com"})

      {:ok, _profile1} = Ash.create(Profile, %{name: "NestedTest", age: 25, bio: "Young"})
      {:ok, _profile2} = Ash.create(Profile, %{name: "NestedTest", age: 35, bio: "Old"})
      {:ok, _profile3} = Ash.create(Profile, %{name: "Other", age: 30, bio: "Same age"})

      # Test nested parentheses with parent()
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
      {:ok, user} = Ash.create(User, %{name: "StringTest", email: "string@example.com"})

      {:ok, _profile1} =
        Ash.create(Profile, %{name: "StringTest", bio: "StringTest is mentioned here"})

      {:ok, _profile2} =
        Ash.create(Profile, %{name: "DifferentName", bio: "StringTest appears in bio"})

      {:ok, _profile3} = Ash.create(Profile, %{name: "StringTest", bio: "No mention"})

      # Test parent() with string contains function
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
