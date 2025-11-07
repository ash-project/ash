# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Resource.AggregatesTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Resource.Aggregate
  alias Ash.Test.Domain, as: Domain

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
    test "aggregates with multitenancy :bypass can count across all tenants" do
      require Ash.Query

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

          # Read action with multitenancy bypass for comparison
          read :read_all_tenants do
            multitenancy :bypass
          end
        end

        relationships do
          has_many :comments, MultitenantComment, destination_attribute: :post_id, public?: true
        end

        aggregates do
          # This aggregate bypasses multitenancy and counts comments across all tenants
          count :total_comments_all_tenants, :comments do
            public?(true)
            multitenancy :bypass
          end

          # This aggregate respects multitenancy and only counts current tenant's comments
          count :total_comments_current_tenant, :comments do
            public?(true)
          end

          # Bypass aggregate that counts only active comments across all tenants
          count :active_comments_all_tenants, :comments do
            public?(true)
            multitenancy :bypass
            filter expr(status == "active")
          end

          # Normal aggregate that counts only active comments in current tenant
          count :active_comments_current_tenant, :comments do
            public?(true)
            filter expr(status == "active")
          end
        end
      end

      tenant1_post =
        MultitenantPost
        |> Ash.Changeset.for_create(:create, %{title: "T1 Post"}, tenant: "tenant1")
        |> Ash.create!()

      _tenant2_post =
        MultitenantPost
        |> Ash.Changeset.for_create(:create, %{title: "T2 Post"}, tenant: "tenant2")
        |> Ash.create!()

      for tenant <- ["tenant1", "tenant2"] do
        for _i <- 1..2 do
          MultitenantComment
          |> Ash.Changeset.for_create(
            :create,
            %{post_id: tenant1_post.id, status: "active"},
            tenant: tenant
          )
          |> Ash.create!()
        end

        # One inactive comment per tenant
        MultitenantComment
        |> Ash.Changeset.for_create(
          :create,
          %{post_id: tenant1_post.id, status: "inactive"},
          tenant: tenant
        )
        |> Ash.create!()
      end

      # Test 1: Load aggregates directly from a resource instance
      post_with_aggregates =
        Ash.load!(
          tenant1_post,
          [
            :total_comments_all_tenants,
            :total_comments_current_tenant,
            :active_comments_all_tenants,
            :active_comments_current_tenant
          ],
          tenant: "tenant1"
        )

      # Bypass aggregate sees ALL comments for tenant1_post across all tenants (3 in tenant1 + 3 in tenant2 = 6)
      assert post_with_aggregates.total_comments_all_tenants == 6

      # Normal aggregate only sees comments for tenant1_post in tenant1 (3)
      assert post_with_aggregates.total_comments_current_tenant == 3

      # Bypass aggregate with filter sees active comments for tenant1_post across all tenants (2 in tenant1 + 2 in tenant2 = 4)
      assert post_with_aggregates.active_comments_all_tenants == 4

      # Normal aggregate with filter only sees active comments for tenant1_post in tenant1 (2)
      assert post_with_aggregates.active_comments_current_tenant == 2

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
      require Ash.Query

      # Use the same modules from the previous test
      defmodule MultitenantComment2 do
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
          attribute :status, :string, public?: true
        end

        actions do
          default_accept :*
          defaults [:read, :destroy, update: :*, create: :*]
        end
      end

      defmodule MultitenantPost2 do
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
          defaults [:destroy, update: :*, create: :*]

          # Regular read action
          read :read do
            primary? true
          end

          # Bypass read action
          read :read_bypass do
            multitenancy :bypass
          end
        end

        relationships do
          has_many :comments, MultitenantComment2, destination_attribute: :post_id, public?: true
        end

        aggregates do
          # Aggregate WITH bypass
          count :count_all_tenants, :comments do
            public?(true)
            multitenancy :bypass
          end

          # Aggregate WITHOUT bypass
          count :count_current_tenant, :comments do
            public?(true)
          end
        end
      end

      # Create test data
      post_t1 =
        MultitenantPost2
        |> Ash.Changeset.for_create(:create, %{title: "Post T1"}, tenant: "tenant1")
        |> Ash.create!()

      post_t2 =
        MultitenantPost2
        |> Ash.Changeset.for_create(:create, %{title: "Post T2"}, tenant: "tenant2")
        |> Ash.create!()

      # Create 3 comments in tenant1 for post_t1
      for _i <- 1..3 do
        MultitenantComment2
        |> Ash.Changeset.for_create(:create, %{post_id: post_t1.id}, tenant: "tenant1")
        |> Ash.create!()
      end

      # Create 2 comments in tenant2 for post_t1 (cross-tenant comments)
      for _i <- 1..2 do
        MultitenantComment2
        |> Ash.Changeset.for_create(:create, %{post_id: post_t1.id}, tenant: "tenant2")
        |> Ash.create!()
      end

      # Create 4 comments in tenant2 for post_t2
      for _i <- 1..4 do
        MultitenantComment2
        |> Ash.Changeset.for_create(:create, %{post_id: post_t2.id}, tenant: "tenant2")
        |> Ash.create!()
      end

      # Test Case 1: Action WITHOUT bypass + Aggregates WITH and WITHOUT bypass
      # Action respects tenant, gets only tenant1's post
      [loaded_post] =
        MultitenantPost2
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
        MultitenantPost2
        |> Ash.Query.load([:count_all_tenants])
        |> Ash.read!(action: :read_bypass)

      assert length(all_posts) == 2

      post1 = Enum.find(all_posts, &(&1.id == post_t1.id))
      # All comments for post_t1
      assert post1.count_all_tenants == 5

      post2 = Enum.find(all_posts, &(&1.id == post_t2.id))
      # All comments for post_t2
      assert post2.count_all_tenants == 4

      # Test Case 3: Action WITH bypass + Aggregate WITHOUT bypass
      # This is the complex case - non-bypass aggregates need tenant context
      # Currently this would fail with TenantRequired error, so we document this limitation!
      assert_raise Ash.Error.Invalid, fn ->
        MultitenantPost2
        |> Ash.Query.load([:count_current_tenant])
        |> Ash.read!(action: :read_bypass)
      end
    end
  end
end
