# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Resource.AggregatesTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Resource.Aggregate
  alias Ash.Test.Domain, as: Domain
  require Ash.Query

  defmodule Comment do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    attributes do
      uuid_primary_key :id

      attribute :post_id, :uuid do
        public?(true)
      end
    end

    relationships do
      has_many :likes, Like, destination_attribute: :comment_id, public?: true
    end
  end

  defmodule Like do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    attributes do
      uuid_primary_key :id

      attribute :comment_id, :uuid do
        public?(true)
      end
    end
  end

  defmacrop defposts(do: body) do
    module = Module.concat(["rand#{System.unique_integer([:positive])}", Post])

    quote do
      defmodule unquote(module) do
        @moduledoc false
        use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

        attributes do
          uuid_primary_key :id
        end

        unquote(body)
      end

      alias unquote(module), as: Post
    end
  end

  describe "representation" do
    test "aggregates are persisted on the resource properly" do
      defposts do
        aggregates do
          count :count_of_comments, :comments, public?: true
          count :another_count_but_private, :comments
        end

        relationships do
          has_many :comments, Comment, destination_attribute: :post_id, public?: true
        end
      end

      assert [
               %Aggregate{
                 name: :count_of_comments,
                 kind: :count,
                 relationship_path: [:comments],
                 public?: true
               },
               %Aggregate{
                 name: :another_count_but_private,
                 kind: :count,
                 relationship_path: [:comments],
                 public?: false
               }
             ] = Ash.Resource.Info.aggregates(Post)

      assert [
               %Aggregate{name: :count_of_comments}
             ] = Ash.Resource.Info.public_aggregates(Post)

      assert %Aggregate{name: :another_count_but_private} =
               Ash.Resource.Info.aggregate(Post, :another_count_but_private)

      assert nil == Ash.Resource.Info.public_aggregate(Post, :another_count_but_private)

      assert nil == Ash.Resource.Info.aggregate(Post, :totally_legit_aggregate)
    end

    test "Aggregate descriptions are allowed" do
      defposts do
        aggregates do
          count :count_of_comments, :comments,
            description: "require one of name/contents",
            public?: true
        end

        relationships do
          has_many :comments, Comment, destination_attribute: :post_id, public?: true
        end
      end

      assert [
               %Ash.Resource.Aggregate{description: "require one of name/contents"}
             ] = Ash.Resource.Info.aggregates(Post)
    end

    test "aggregates field should be calculation or attribute on the resource" do
      output =
        ExUnit.CaptureIO.capture_io(:stderr, fn ->
          defposts do
            aggregates do
              sum :sum_of_comment_likes, :comments, :likes do
                public? true
              end
            end

            relationships do
              has_many :comments, Comment, destination_attribute: :post_id, public?: true
            end
          end
        end)

      assert String.contains?(output, "likes") or String.contains?(output, "field")
    end
  end

  test "it can load aggregates on resources which require an explicit domain" do
    defmodule Leg do
      @moduledoc false
      use Ash.Resource,
        domain: nil,
        data_layer: Ash.DataLayer.Ets,
        validate_domain_inclusion?: false

      attributes do
        uuid_primary_key :id
        attribute :side, :string, allow_nil?: false, public?: true
      end

      relationships do
        belongs_to :pants, Pants do
          attribute_writable? true
          public? true
        end
      end

      actions do
        defaults [:create, :read]
        default_accept :*
      end
    end

    defmodule Pants do
      @moduledoc false
      use Ash.Resource,
        domain: nil,
        data_layer: Ash.DataLayer.Ets,
        validate_domain_inclusion?: false

      attributes do
        uuid_primary_key :id
      end

      relationships do
        has_many :legs, Leg
      end

      aggregates do
        count :leg_count, :legs
      end

      actions do
        defaults [:create, :read]
        default_accept :*
      end
    end

    defmodule Clothing do
      @moduledoc false
      use Ash.Domain, validate_config_inclusion?: false

      resources do
        allow_unregistered? true
      end
    end

    pants =
      Pants
      |> Ash.Changeset.for_create(:create, %{}, domain: Clothing)
      |> Ash.create!(domain: Clothing)

    ~w[left right]
    |> Enum.map(fn side ->
      Leg
      |> Ash.Changeset.for_create(:create, %{pants_id: pants.id, side: side}, domain: Clothing)
      |> Ash.create!(domain: Clothing)
    end)

    Ash.load!(pants, :leg_count, domain: Clothing)
  end

  describe "multitenancy bypass" do
    # Define modules once for all tests in this describe block
    defmodule MultitenantComment do
      @moduledoc false
      use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

      multitenancy do
        strategy(:attribute)
        attribute(:tenant_id)
      end

      attributes do
        uuid_primary_key :id

        attribute :tenant_id, :string do
          public?(true)
        end

        attribute :post_id, :uuid do
          public?(true)
        end

        attribute :status, :string do
          public?(true)
        end

        # Add fields for testing different aggregate types
        attribute :rating, :integer do
          public?(true)
        end

        attribute :author_name, :string do
          public?(true)
        end

        attribute :created_at, :utc_datetime do
          public?(true)
        end
      end

      actions do
        default_accept :*
        defaults [:read, :destroy, update: :*, create: :*]
      end

      relationships do
        belongs_to :post, MultitenantPost do
          public?(true)
        end
      end
    end

    defmodule MultitenantPost do
      @moduledoc false
      use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

      multitenancy do
        strategy(:attribute)
        attribute(:tenant_id)
      end

      attributes do
        uuid_primary_key :id

        attribute :tenant_id, :string do
          public?(true)
        end

        attribute :title, :string do
          public?(true)
        end
      end

      actions do
        default_accept :*
        defaults [:destroy, update: :*, create: :*]

        # Default read action without multitenancy bypass
        read :read do
          primary? true
        end

        # Read action with multitenancy bypass
        read :read_all_tenants do
          multitenancy :bypass
        end

        # Renamed bypass action for second test
        read :read_bypass do
          multitenancy :bypass
        end
      end

      relationships do
        has_many :comments, MultitenantComment, destination_attribute: :post_id, public?: true
      end

      aggregates do
        # COUNT Aggregates WITH bypass
        count :total_comments_all_tenants, :comments do
          public?(true)
          multitenancy :bypass
        end

        count :active_comments_all_tenants, :comments do
          public?(true)
          multitenancy :bypass
          filter expr(status == "active")
        end

        # For second test - alias names
        count :count_all_tenants, :comments do
          public?(true)
          multitenancy :bypass
        end

        # EXISTS Aggregate WITH bypass
        exists :has_comments_all_tenants, :comments do
          public?(true)
          multitenancy :bypass
        end

        # FIRST Aggregate WITH bypass
        first :first_comment_author_all_tenants, :comments, :author_name do
          public?(true)
          multitenancy :bypass
        end

        # SUM Aggregate WITH bypass
        sum :total_rating_all_tenants, :comments, :rating do
          public?(true)
          multitenancy :bypass
        end

        # LIST Aggregate WITH bypass
        list :comment_authors_all_tenants, :comments, :author_name do
          public?(true)
          multitenancy :bypass
        end

        # MAX Aggregate WITH bypass
        max :max_rating_all_tenants, :comments, :rating do
          public?(true)
          multitenancy :bypass
        end

        # MIN Aggregate WITH bypass
        min :min_rating_all_tenants, :comments, :rating do
          public?(true)
          multitenancy :bypass
        end

        # AVG Aggregate WITH bypass
        avg :avg_rating_all_tenants, :comments, :rating do
          public?(true)
          multitenancy :bypass
        end

        # COUNT Aggregates WITHOUT bypass
        count :total_comments_current_tenant, :comments do
          public?(true)
        end

        count :active_comments_current_tenant, :comments do
          public?(true)
          filter expr(status == "active")
        end

        # For second test - alias name
        count :count_current_tenant, :comments do
          public?(true)
        end

        # EXISTS Aggregate WITHOUT bypass
        exists :has_comments_current_tenant, :comments do
          public?(true)
        end

        # FIRST Aggregate WITHOUT bypass
        first :first_comment_author_current_tenant, :comments, :author_name do
          public?(true)
        end

        # SUM Aggregate WITHOUT bypass
        sum :total_rating_current_tenant, :comments, :rating do
          public?(true)
        end

        # LIST Aggregate WITHOUT bypass
        list :comment_authors_current_tenant, :comments, :author_name do
          public?(true)
        end

        # MAX Aggregate WITHOUT bypass
        max :max_rating_current_tenant, :comments, :rating do
          public?(true)
        end

        # MIN Aggregate WITHOUT bypass
        min :min_rating_current_tenant, :comments, :rating do
          public?(true)
        end

        # AVG Aggregate WITHOUT bypass
        avg :avg_rating_current_tenant, :comments, :rating do
          public?(true)
        end
      end
    end

    test "aggregates with multitenancy :bypass can count across all tenants" do
      require Ash.Query

      tenant1_post =
        MultitenantPost
        |> Ash.Changeset.for_create(:create, %{title: "T1 Post"}, tenant: "tenant1")
        |> Ash.create!()

      _tenant2_post =
        MultitenantPost
        |> Ash.Changeset.for_create(:create, %{title: "T2 Post"}, tenant: "tenant2")
        |> Ash.create!()

      for {tenant, idx} <- Enum.with_index(["tenant1", "tenant2"]) do
        # Create 2 active comments per tenant with varying ratings and authors
        for i <- 1..2 do
          MultitenantComment
          |> Ash.Changeset.for_create(
            :create,
            %{
              post_id: tenant1_post.id,
              status: "active",
              # tenant1: 5,10  tenant2: 15,20
              rating: idx * 10 + i * 5,
              author_name: "Author#{idx}#{i}",
              created_at: DateTime.utc_now() |> DateTime.add(idx * 100 + i, :second)
            },
            tenant: tenant
          )
          |> Ash.create!()
        end

        # One inactive comment per tenant with specific ratings
        MultitenantComment
        |> Ash.Changeset.for_create(
          :create,
          %{
            post_id: tenant1_post.id,
            status: "inactive",
            # tenant1: 25  tenant2: 35
            rating: idx * 10 + 25,
            author_name: "InactiveAuthor#{idx}",
            created_at: DateTime.utc_now() |> DateTime.add(idx * 100 + 50, :second)
          },
          tenant: tenant
        )
        |> Ash.create!()
      end

      # Test 1: Load ALL aggregate types directly from a resource instance
      post_with_aggregates =
        Ash.load!(
          tenant1_post,
          [
            # Count aggregates
            :total_comments_all_tenants,
            :total_comments_current_tenant,
            :active_comments_all_tenants,
            :active_comments_current_tenant,
            # Exists aggregates
            :has_comments_all_tenants,
            :has_comments_current_tenant,
            # First aggregates
            :first_comment_author_all_tenants,
            :first_comment_author_current_tenant,
            # Sum aggregates
            :total_rating_all_tenants,
            :total_rating_current_tenant,
            # List aggregates
            :comment_authors_all_tenants,
            :comment_authors_current_tenant,
            # Max aggregates
            :max_rating_all_tenants,
            :max_rating_current_tenant,
            # Min aggregates
            :min_rating_all_tenants,
            :min_rating_current_tenant,
            # Avg aggregates
            :avg_rating_all_tenants,
            :avg_rating_current_tenant
          ],
          tenant: "tenant1"
        )

      # COUNT: Bypass sees ALL (3 in tenant1 + 3 in tenant2 = 6), Normal sees only tenant1 (3)
      assert post_with_aggregates.total_comments_all_tenants == 6
      assert post_with_aggregates.total_comments_current_tenant == 3
      assert post_with_aggregates.active_comments_all_tenants == 4
      assert post_with_aggregates.active_comments_current_tenant == 2

      # EXISTS: Both should be true as there are comments
      assert post_with_aggregates.has_comments_all_tenants == true
      assert post_with_aggregates.has_comments_current_tenant == true

      # FIRST: First author name (depends on sort order)
      assert post_with_aggregates.first_comment_author_all_tenants != nil
      assert post_with_aggregates.first_comment_author_current_tenant != nil

      # SUM: tenant1 ratings: 5+10+25=40, tenant2 ratings: 15+20+35=70, total: 110
      assert post_with_aggregates.total_rating_all_tenants == 110
      assert post_with_aggregates.total_rating_current_tenant == 40

      # LIST: All author names
      all_authors = post_with_aggregates.comment_authors_all_tenants
      assert length(all_authors) == 6

      assert Enum.sort(all_authors) == [
               "Author01",
               "Author02",
               "Author11",
               "Author12",
               "InactiveAuthor0",
               "InactiveAuthor1"
             ]

      tenant1_authors = post_with_aggregates.comment_authors_current_tenant
      assert length(tenant1_authors) == 3
      assert Enum.sort(tenant1_authors) == ["Author01", "Author02", "InactiveAuthor0"]

      # MAX: Maximum rating - all tenants: 35, tenant1: 25
      assert post_with_aggregates.max_rating_all_tenants == 35
      assert post_with_aggregates.max_rating_current_tenant == 25

      # MIN: Minimum rating - all tenants: 5, tenant1: 5
      assert post_with_aggregates.min_rating_all_tenants == 5
      assert post_with_aggregates.min_rating_current_tenant == 5

      # AVG: Average rating - all: 110/6≈18.33, tenant1: 40/3≈13.33
      assert_in_delta post_with_aggregates.avg_rating_all_tenants, 18.33, 0.01
      assert_in_delta post_with_aggregates.avg_rating_current_tenant, 13.33, 0.01

      # Test 2: Load aggregates through a regular read action (without bypass)
      # This tests that aggregate bypass works independently of action bypass
      [post_from_read] =
        MultitenantPost
        |> Ash.Query.filter(id == ^tenant1_post.id)
        |> Ash.Query.load([
          :total_comments_all_tenants,
          :total_comments_current_tenant
        ])
        |> Ash.read!(action: :read, tenant: "tenant1")

      # Even though the read action doesn't bypass, the aggregate with bypass should still see all tenants
      assert post_from_read.total_comments_all_tenants == 6
      assert post_from_read.total_comments_current_tenant == 3

      # Test 3: Load only bypass aggregates through a read action WITH bypass
      # Non-bypass aggregates cannot be loaded through bypass actions without a tenant
      [post_from_bypass_read] =
        MultitenantPost
        |> Ash.Query.filter(id == ^tenant1_post.id)
        |> Ash.Query.load([
          :total_comments_all_tenants,
          :active_comments_all_tenants
        ])
        |> Ash.read!(action: :read_all_tenants)

      # Bypass aggregates should work correctly with bypass read action
      assert post_from_bypass_read.total_comments_all_tenants == 6
      assert post_from_bypass_read.active_comments_all_tenants == 4

      # Test 4: Load all posts with bypass aggregates through bypass action
      # This tests that bypass aggregates work when loading multiple resources
      all_posts_with_bypass =
        MultitenantPost
        |> Ash.Query.filter(title in ["T1 Post", "T2 Post"])
        |> Ash.Query.load([
          :total_comments_all_tenants,
          :active_comments_all_tenants
        ])
        |> Ash.read!(action: :read_all_tenants)

      # We should get both posts since the read action bypasses tenant
      assert length(all_posts_with_bypass) == 2

      # Find the tenant1_post in the results
      loaded_tenant1_post = Enum.find(all_posts_with_bypass, &(&1.id == tenant1_post.id))
      assert loaded_tenant1_post.total_comments_all_tenants == 6
      assert loaded_tenant1_post.active_comments_all_tenants == 4

      # The tenant2_post should have 0 comments since we only created comments for tenant1_post
      loaded_tenant2_post = Enum.find(all_posts_with_bypass, &(&1.id != tenant1_post.id))
      assert loaded_tenant2_post.total_comments_all_tenants == 0
      assert loaded_tenant2_post.active_comments_all_tenants == 0
    end

    test "aggregates behavior with different action and aggregate multitenancy combinations" do
      # Create test data
      post_t1 =
        MultitenantPost
        |> Ash.Changeset.for_create(:create, %{title: "Post T1"}, tenant: "tenant1")
        |> Ash.create!()

      post_t2 =
        MultitenantPost
        |> Ash.Changeset.for_create(:create, %{title: "Post T2"}, tenant: "tenant2")
        |> Ash.create!()

      # Create 3 comments in tenant1 for post_t1
      for _i <- 1..3 do
        MultitenantComment
        |> Ash.Changeset.for_create(:create, %{post_id: post_t1.id, status: "active"},
          tenant: "tenant1"
        )
        |> Ash.create!()
      end

      # Create 2 comments in tenant2 for post_t1 (cross-tenant comments)
      for _i <- 1..2 do
        MultitenantComment
        |> Ash.Changeset.for_create(:create, %{post_id: post_t1.id, status: "active"},
          tenant: "tenant2"
        )
        |> Ash.create!()
      end

      # Create 4 comments in tenant2 for post_t2
      for _i <- 1..4 do
        MultitenantComment
        |> Ash.Changeset.for_create(:create, %{post_id: post_t2.id, status: "active"},
          tenant: "tenant2"
        )
        |> Ash.create!()
      end

      # Test Case 1: Action WITHOUT bypass + Aggregates WITH and WITHOUT bypass
      # Action respects tenant, gets only tenant1's post
      [loaded_post] =
        MultitenantPost
        |> Ash.Query.filter(id == ^post_t1.id)
        |> Ash.Query.load([:count_all_tenants, :count_current_tenant])
        |> Ash.read!(action: :read, tenant: "tenant1")

      assert loaded_post.id == post_t1.id
      # Bypass aggregate sees ALL comments for this post across all tenants (3 + 2 = 5)
      assert loaded_post.count_all_tenants == 5
      # Non-bypass aggregate sees only tenant1 comments (3)
      assert loaded_post.count_current_tenant == 3

      # Test Case 2: Action WITH bypass + Aggregate WITH bypass
      # Action gets all posts, bypass aggregate counts all
      all_posts =
        MultitenantPost
        |> Ash.Query.filter(title in ["Post T1", "Post T2"])
        |> Ash.Query.load([:count_all_tenants])
        |> Ash.read!(action: :read_bypass)

      assert length(all_posts) == 2

      post1 = Enum.find(all_posts, &(&1.id == post_t1.id))
      # All comments for post_t1
      assert post1.count_all_tenants == 5

      post2 = Enum.find(all_posts, &(&1.id == post_t2.id))
      # All comments for post_t2
      assert post2.count_all_tenants == 4

      # Test Case 3: Mixed aggregates (bypass and non-bypass) with regular action
      # With our fix, each aggregate should use its own tenant context
      [mixed_post] =
        MultitenantPost
        |> Ash.Query.filter(id == ^post_t1.id)
        |> Ash.Query.load([:count_all_tenants, :count_current_tenant])
        |> Ash.read!(action: :read, tenant: "tenant1")

      assert mixed_post.id == post_t1.id

      # Bypass aggregate should count ALL (5 comments across both tenants)
      assert mixed_post.count_all_tenants == 5

      # Non-bypass aggregate should count only tenant1 (3 comments)
      assert mixed_post.count_current_tenant == 3

      # Test Case 4: Action WITH bypass + Aggregate WITHOUT bypass
      # Solution: Pass tenant via opts for non-bypass aggregates
      # The bypass action sees all posts, but aggregates can still use specific tenant
      all_posts_with_tenant_agg =
        MultitenantPost
        |> Ash.Query.filter(title in ["Post T1", "Post T2"])
        # Non-bypass aggregate
        |> Ash.Query.load([:count_current_tenant, :count_all_tenants])
        # Pass tenant in opts
        |> Ash.read!(action: :read_bypass, tenant: "tenant1")

      # Should get both posts (bypass action)
      assert length(all_posts_with_tenant_agg) == 2

      post1_with_tenant = Enum.find(all_posts_with_tenant_agg, &(&1.id == post_t1.id))
      # Non-bypass aggregate uses the tenant from opts (tenant1), sees only 3 comments
      assert post1_with_tenant.count_current_tenant == 3
      assert post1_with_tenant.count_all_tenants == 5

      post2_with_tenant = Enum.find(all_posts_with_tenant_agg, &(&1.id == post_t2.id))
      # Post T2 has no comments in tenant1
      assert post2_with_tenant.count_current_tenant == 0
      assert post2_with_tenant.count_all_tenants == 4

      # Test Case 5: Without tenant in opts
      # This is the limit of bypass actions; it should still error
      assert_raise Ash.Error.Invalid, fn ->
        MultitenantPost
        |> Ash.Query.load([:count_current_tenant])
        |> Ash.read!(action: :read_bypass)
      end
    end

    test "returns error when main resource uses context multitenancy with bypass aggregate" do
      defmodule ContextPost do
        @moduledoc false
        use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

        multitenancy do
          strategy(:context)
        end

        attributes do
          uuid_primary_key :id
          attribute :title, :string, public?: true
        end

        actions do
          default_accept :*
          defaults [:read, :destroy, update: :*, create: :*]

          read :read_bypass do
            multitenancy :bypass
          end
        end

        relationships do
          has_many :comments, MultitenantComment, destination_attribute: :post_id, public?: true
        end

        aggregates do
          count :comment_count_bypass, :comments do
            public?(true)
            multitenancy :bypass
          end
        end
      end

      # Should return error because ContextPost uses :context strategy
      assert {:error, error} =
               ContextPost
               |> Ash.Query.load([:comment_count_bypass])
               |> Ash.read(domain: Domain, action: :read_bypass)

      assert Exception.message(error) =~ "uses `:context` multitenancy strategy"
    end

    test "works when main resource uses context multitenancy with normal aggregate (no bypass)" do
      defmodule ContextPostNormal do
        @moduledoc false
        use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

        multitenancy do
          strategy(:context)
        end

        attributes do
          uuid_primary_key :id
          attribute :title, :string, public?: true
        end

        actions do
          default_accept :*
          defaults [:read, :destroy, update: :*, create: :*]
        end

        relationships do
          has_many :comments, MultitenantComment, destination_attribute: :post_id, public?: true
        end

        aggregates do
          # No bypass - should work fine
          count :comment_count_normal, :comments do
            public?(true)
          end
        end
      end

      # Should work because aggregate doesn't use bypass
      result =
        ContextPostNormal
        |> Ash.Query.load([:comment_count_normal])
        |> Ash.read!(domain: Domain, tenant: "test_tenant")

      assert is_list(result)
    end

    test "returns error when relationship path includes context multitenancy resource with bypass" do
      defmodule ContextLike do
        @moduledoc false
        use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

        multitenancy do
          strategy(:context)
        end

        attributes do
          uuid_primary_key :id
          attribute :comment_id, :uuid, public?: true
        end

        actions do
          default_accept :*
          defaults [:read, :destroy, update: :*, create: :*]
        end
      end

      defmodule AttributeComment do
        @moduledoc false
        use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

        multitenancy do
          strategy(:attribute)
          attribute(:tenant_id)
        end

        attributes do
          uuid_primary_key :id
          attribute :tenant_id, :string, public?: true
          attribute :post_id, :uuid, public?: true
        end

        actions do
          default_accept :*
          defaults [:read, :destroy, update: :*, create: :*]
        end

        relationships do
          has_many :likes, ContextLike, destination_attribute: :comment_id, public?: true
        end
      end

      defmodule AttributePost do
        @moduledoc false
        use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

        multitenancy do
          strategy(:attribute)
          attribute(:tenant_id)
        end

        attributes do
          uuid_primary_key :id
          attribute :tenant_id, :string, public?: true
          attribute :title, :string, public?: true
        end

        actions do
          default_accept :*
          defaults [:read, :destroy, update: :*, create: :*]
        end

        relationships do
          has_many :comments, AttributeComment, destination_attribute: :post_id, public?: true
        end

        aggregates do
          # This should fail: path is [:comments, :likes] where likes uses :context
          count :like_count_bypass, [:comments, :likes] do
            public?(true)
            multitenancy :bypass
          end
        end
      end

      # Should return error because ContextLike (in path) uses :context strategy
      assert {:error, error} =
               AttributePost
               |> Ash.Query.load([:like_count_bypass])
               |> Ash.read(domain: Domain, tenant: "tenant1")

      assert Exception.message(error) =~ "in relationship `likes`"
      assert Exception.message(error) =~ "uses `:context` multitenancy strategy"
    end

    test "works when bypass aggregate avoids context multitenancy resource in path" do
      defmodule MixedContextLike do
        @moduledoc false
        use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

        multitenancy do
          strategy(:context)
        end

        attributes do
          uuid_primary_key :id
          attribute :comment_id, :uuid, public?: true
        end

        actions do
          default_accept :*
          defaults [:read, :destroy, update: :*, create: :*]
        end
      end

      defmodule MixedAttributeComment do
        @moduledoc false
        use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

        multitenancy do
          strategy(:attribute)
          attribute(:tenant_id)
        end

        attributes do
          uuid_primary_key :id
          attribute :tenant_id, :string, public?: true
          attribute :post_id, :uuid, public?: true
        end

        actions do
          default_accept :*
          defaults [:read, :destroy, update: :*, create: :*]
        end

        relationships do
          has_many :likes, MixedContextLike, destination_attribute: :comment_id, public?: true
        end
      end

      defmodule MixedAttributePost do
        @moduledoc false
        use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

        multitenancy do
          strategy(:attribute)
          attribute(:tenant_id)
        end

        attributes do
          uuid_primary_key :id
          attribute :tenant_id, :string, public?: true
          attribute :title, :string, public?: true
        end

        actions do
          default_accept :*
          defaults [:read, :destroy, update: :*, create: :*]
        end

        relationships do
          has_many :comments, MixedAttributeComment,
            destination_attribute: :post_id,
            public?: true
        end

        aggregates do
          # This should work: only uses [:comments] path, avoids :likes (which is context)
          count :comment_count_bypass, :comments do
            public?(true)
            multitenancy :bypass
          end

          # This should work: normal aggregate without bypass can use context resources
          count :like_count_normal, [:comments, :likes] do
            public?(true)
          end
        end
      end

      # Create test data
      post =
        MixedAttributePost
        |> Ash.Changeset.for_create(:create, %{title: "Test"}, tenant: "tenant1")
        |> Ash.create!(domain: Domain)

      _comment =
        MixedAttributeComment
        |> Ash.Changeset.for_create(:create, %{post_id: post.id}, tenant: "tenant1")
        |> Ash.create!(domain: Domain)

      # Should work: bypass aggregate only uses [:comments] path (all attribute strategy)
      result =
        MixedAttributePost
        |> Ash.Query.filter(id == ^post.id)
        |> Ash.Query.load([:comment_count_bypass, :like_count_normal])
        |> Ash.read!(domain: Domain, tenant: "tenant1")

      assert [loaded_post] = result
      assert loaded_post.comment_count_bypass == 1
      assert loaded_post.like_count_normal == 0
    end
  end
end
