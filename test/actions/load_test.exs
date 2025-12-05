# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Actions.LoadTest do
  @moduledoc false
  use ExUnit.Case, async: false

  require Ash.Query

  alias Ash.Test.Domain, as: Domain

  defmodule ExampleWithNoPrimaryRead do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets

    actions do
      defaults [:create]
    end

    attributes do
      uuid_primary_key :id
    end

    calculations do
      calculate :id_calc, :uuid, expr(id)
    end
  end

  defmodule Campaign do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults([:read, :destroy, create: :*, update: :*])
    end

    attributes do
      uuid_primary_key(:id)
      attribute(:name, :ci_string, public?: true)
    end
  end

  defmodule Bio do
    use Ash.Resource,
      authorizers: [Ash.Policy.Authorizer],
      data_layer: :embedded

    attributes do
      attribute(:first_name, :string, public?: true)
      attribute(:last_name, :string, public?: true)

      attribute :type, :string do
        allow_nil?(false)
        default("bio")
      end

      attribute :forbidden_field, :string do
        public?(true)
      end
    end

    field_policies do
      field_policy :* do
        authorize_if always()
      end

      field_policy :forbidden_field do
        forbid_if always()
      end

      field_policy :forbidden_name do
        forbid_if always()
      end
    end

    policies do
      policy always() do
        authorize_if always()
      end
    end

    calculations do
      calculate :full_name, :string, expr(first_name <> " " <> last_name) do
        public?(true)
      end

      calculate :forbidden_name, :string, expr(first_name <> " " <> last_name) do
        public?(true)
      end
    end
  end

  defmodule OtherKindOfBio do
    use Ash.Resource,
      authorizers: [Ash.Policy.Authorizer],
      data_layer: :embedded

    attributes do
      attribute(:first_name, :string, public?: true)
      attribute(:last_name, :string, public?: true)

      attribute :type, :string do
        allow_nil?(false)
        default("other_kind_of_bio")
      end

      attribute :forbidden_field, :string do
        public?(true)
      end
    end

    field_policies do
      field_policy :* do
        authorize_if always()
      end

      field_policy :forbidden_field do
        forbid_if always()
      end

      field_policy :forbidden_name do
        forbid_if always()
      end
    end

    policies do
      policy always() do
        authorize_if always()
      end
    end

    calculations do
      calculate :full_name, :string, expr(first_name <> " " <> last_name) do
        public?(true)
      end

      calculate :forbidden_name, :string, expr(first_name <> " " <> last_name) do
        public?(true)
      end
    end
  end

  defmodule BioUnion do
    use Ash.Type.NewType,
      subtype_of: :union,
      constraints: [
        types: [
          bio: [
            type: Bio,
            tag: :type,
            tag_value: "bio"
          ],
          other_kind_of_bio: [
            type: OtherKindOfBio,
            tag: :type,
            tag_value: "other_kind_of_bio"
          ]
        ]
      ]
  end

  defmodule PostsWithACalc do
    use Ash.Resource.Calculation

    def load(_, _, _), do: [posts: :category_length]

    def calculate(records, _, _) do
      Enum.map(records, & &1.posts)
    end
  end

  defmodule Author do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [
        Ash.Test.Authorizer
      ]

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults([:read, :destroy, create: :*, update: :*])
    end

    attributes do
      uuid_primary_key(:id)
      attribute(:name, :string, public?: true)
      attribute(:bio, Bio, public?: true)
      attribute(:bio_union, BioUnion, public?: true)
    end

    calculations do
      calculate :bio_union_calc, BioUnion, expr(bio_union) do
        public?(true)
      end

      calculate :campaign_upcase, :string, Ash.Test.Actions.LoadTest.UpcaseName

      calculate :posts_calc, :struct, PostsWithACalc do
        constraints instance_of: Ash.Test.Actions.LoadTest.Post
      end
    end

    aggregates do
      count :posts_count, :posts
    end

    relationships do
      has_many(:posts, Ash.Test.Actions.LoadTest.Post,
        destination_attribute: :author_id,
        public?: true
      )

      has_one(:latest_post, Ash.Test.Actions.LoadTest.Post,
        destination_attribute: :author_id,
        sort: [inserted_at: :desc],
        public?: true
      )

      belongs_to :campaign, Ash.Test.Actions.LoadTest.Campaign do
        attribute_type(:ci_string)
        source_attribute(:campaign_name)
        destination_attribute(:name)
        public?(true)
      end
    end
  end

  defmodule UpcaseName do
    use Ash.Resource.Calculation

    @impl true
    def load(_, _, _), do: [campaign: :name]

    @impl true
    def calculate(authors, _, _) do
      Enum.map(authors, fn author ->
        author.campaign.name
        |> to_string()
        |> String.upcase()
      end)
    end
  end

  defmodule PostsInSameCategory do
    use Ash.Resource.ManualRelationship

    def load(posts, _, %{query: destination_query, domain: domain}) do
      categories = Enum.map(posts, & &1.category)

      other_posts =
        destination_query
        |> Ash.Query.filter(category in ^categories)
        |> Ash.read!(domain: domain)
        |> Enum.group_by(& &1.category)

      {:ok,
       Map.new(posts, fn post ->
         related_posts =
           other_posts
           |> Map.get(post.category, [])
           |> Enum.reject(&(&1.id == post.id))

         {Map.take(post, [:id]), related_posts}
       end)}
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      authorizers: [Ash.Policy.Authorizer],
      data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults([:read, :destroy, create: :*, update: :*])

      read :keyset do
        pagination do
          required? false
          keyset? true
          countable true
        end
      end

      read :required_pagination do
        pagination do
          offset? true
          default_limit 20
          countable :by_default
        end
      end

      read :all_access
    end

    preparations do
      prepare build(load: [:category_length, :sum])
    end

    attributes do
      uuid_primary_key(:id)
      attribute(:title, :string, public?: true)
      attribute(:contents, :string, public?: true)
      attribute(:category, :string, public?: true)
      attribute(:secret, :string, public?: true)
      timestamps()
    end

    policies do
      policy always() do
        authorize_if always()
      end
    end

    field_policies do
      field_policy :* do
        authorize_if always()
      end

      field_policy :secret do
        authorize_if action([:create, :all_access])
      end
    end

    code_interface do
      define :get_by_id do
        action(:read)
        get_by([:id])
      end
    end

    calculations do
      calculate :category_length, :string, expr(string_length(category))

      calculate :sum, :integer do
        argument :a, :integer, default: 1
        argument :b, :integer, default: 2
        calculation expr(^arg(:a) + ^arg(:b))
      end
    end

    relationships do
      belongs_to :author, Author do
        public?(true)
      end

      has_many :posts_in_same_category, __MODULE__ do
        public?(true)
        manual(PostsInSameCategory)
      end

      has_many :ratings, Ash.Test.Actions.LoadTest.Rating do
        public?(true)
        domain(Ash.Test.Actions.LoadTest.Domain2)
      end

      has_many :ratings_after_creation, Ash.Test.Actions.LoadTest.Rating do
        public?(true)
        domain(Ash.Test.Actions.LoadTest.Domain2)
        no_attributes? true
        filter expr(parent(inserted_at) < made_at and parent(id) == post_id)
      end

      has_many :posts_with_same_title, __MODULE__ do
        public?(true)
        no_attributes? true
        filter expr(parent(title) == title and parent(id) != id)
      end

      many_to_many(:categories, Ash.Test.Actions.LoadTest.Category,
        public?: true,
        through: Ash.Test.Actions.LoadTest.PostCategory,
        destination_attribute_on_join_resource: :category_id,
        source_attribute_on_join_resource: :post_id
      )
    end
  end

  defmodule PostCategory do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults([:read, :destroy, create: :*, update: :*])
    end

    relationships do
      belongs_to(:post, Post, primary_key?: true, allow_nil?: false, public?: true)

      belongs_to(:category, Ash.Test.Actions.LoadTest.Category,
        primary_key?: true,
        allow_nil?: false,
        public?: true
      )
    end
  end

  defmodule LinkedCategory do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults([:read, :destroy, create: :*, update: :*])
    end

    attributes do
      attribute(:source_name, :string, public?: true, primary_key?: true, allow_nil?: false)
      attribute(:dest_name, :string, public?: true, primary_key?: true, allow_nil?: false)
    end
  end

  defmodule Category do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults([:read, :destroy, create: :*, update: :*])

      read :keyset do
        pagination do
          required? false
          keyset? true
        end
      end
    end

    attributes do
      uuid_primary_key(:id)
      attribute(:name, :string, public?: true)
    end

    relationships do
      many_to_many(:posts, Post,
        public?: true,
        through: PostCategory,
        destination_attribute_on_join_resource: :post_id,
        source_attribute_on_join_resource: :category_id
      )

      many_to_many :linked_categories, __MODULE__,
        public?: true,
        through: LinkedCategory,
        source_attribute: :name,
        destination_attribute: :name,
        destination_attribute_on_join_resource: :dest_name,
        source_attribute_on_join_resource: :source_name
    end
  end

  defmodule Rating do
    use Ash.Resource,
      domain: Ash.Test.Actions.LoadTest.Domain2,
      data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key(:id)
      attribute(:rating, :integer, public?: true)
      # calling it `made_at` is part of a regression
      # test that this attribute has no `timestamps` (specifically that
      # `inserted_at` reference would not resolve for this resource
      # while on `Post` it does).
      create_timestamp :made_at, public?: true, writable?: true
    end

    actions do
      default_accept :*
      defaults([:read, :destroy, create: :*, update: :*])
    end

    relationships do
      belongs_to :post, Post do
        public?(true)
        domain(Domain)
      end
    end
  end

  defmodule Domain2 do
    @moduledoc false
    use Ash.Domain

    resources do
      resource Rating
    end
  end

  defmodule EventLog do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      defaults([:read, create: [:name, :author_id]])
    end

    attributes do
      attribute(:name, :string, public?: true)
      timestamps()
    end

    relationships do
      belongs_to :author, Author do
        public?(true)
      end
    end

    resource do
      require_primary_key? false
    end
  end

  describe "loads" do
    setup do
      start_supervised(
        {Ash.Test.Authorizer,
         strict_check: :authorized,
         check: {:error, Ash.Error.Forbidden.exception([])},
         strict_check_context: [:query]}
      )

      :ok
    end

    test "it allows loading manual relationships" do
      post1 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post1", category: "foo"})
        |> Ash.create!()

      Post
      |> Ash.Changeset.for_create(:create, %{title: "post2", category: "bar"})
      |> Ash.create!()

      post3 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post2", category: "foo"})
        |> Ash.create!()

      post3_id = post3.id

      assert [%{id: ^post3_id}] =
               post1
               |> Ash.load!(:posts_in_same_category)
               |> Map.get(:posts_in_same_category)
    end

    test "parent references work with no_attributes? true" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post1", category: "foo"})
        |> Ash.create!()

      rating =
        Rating
        |> Ash.Changeset.for_create(:create, %{
          rating: 1,
          made_at: DateTime.shift(post.inserted_at, day: 1)
        })
        |> Ash.Changeset.manage_relationship(:post, post, type: :append_and_remove)
        |> Ash.create!()

      rating_id = rating.id

      assert [%{id: ^rating_id}] =
               Rating
               |> Ash.Query.load(post: [:ratings_after_creation])
               |> Ash.read_one!()
               |> Map.get(:post)
               |> Map.get(:ratings_after_creation)
    end

    test "parent expressions can be used for complex constraints" do
      post1 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post1", category: "foo"})
        |> Ash.create!()

      post1_same =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post1", category: "bar"})
        |> Ash.create!()

      Post
      |> Ash.Changeset.for_create(:create, %{title: "post2", category: "baz"})
      |> Ash.create!()

      post1_same_id = post1_same.id

      assert [%{id: ^post1_same_id}] =
               post1
               |> Ash.load!(:posts_with_same_title)
               |> Map.get(:posts_with_same_title)
    end

    test "it allows loading through manual relationships" do
      post1 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post1", category: "foo"})
        |> Ash.create!()

      Post
      |> Ash.Changeset.for_create(:create, %{title: "post2", category: "bar"})
      |> Ash.create!()

      post3 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post2", category: "foo"})
        |> Ash.create!()

      post3_id = post3.id

      assert [%{id: ^post3_id}] =
               post1
               |> Ash.load!(posts_in_same_category: :ratings)
               |> Map.get(:posts_in_same_category)
    end

    test "it uses `Comp.equal?/2` to support things like ci_string foreign keys" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "zerg", campaign_name: "FrEd"})
        |> Ash.create!()

      Campaign
      |> Ash.Changeset.for_create(:create, %{name: "fReD"})
      |> Ash.create!()

      assert %{
               campaign: %{name: %Ash.CiString{string: "fReD"}},
               campaign_name: %Ash.CiString{string: "FrEd"}
             } = Ash.load!(author, :campaign)
    end

    test "it allows loading data" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "zerg"})
        |> Ash.create!()

      post1 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post1", author_id: author.id})
        |> Ash.create!()

      post2 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post2", author_id: author.id})
        |> Ash.create!()

      assert [fetched_post1, fetched_post2] =
               author
               |> Ash.load!(:posts)
               |> Map.get(:posts)

      assert Enum.sort([post1.id, post2.id]) == Enum.sort([fetched_post1.id, fetched_post2.id])
    end

    test "it allows loading nested related data" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "zerg"})
        |> Ash.create!()

      post1 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post1", author_id: author.id})
        |> Ash.create!()

      post2 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post2", author_id: author.id})
        |> Ash.create!()

      [author] =
        Author
        |> Ash.Query.for_read(:read, %{}, load: [posts: [:author]])
        |> Ash.Query.filter(posts.id == ^post1.id)
        |> Ash.read!(authorize?: true)

      assert Enum.sort(Enum.map(author.posts, &Map.get(&1, :id))) ==
               Enum.sort([post1.id, post2.id])

      for post <- author.posts do
        assert post.author.id == author.id
      end
    end

    test "it allows using a custom read action for related data" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "zerg"})
        |> Ash.create!()

      Post
      |> Ash.Changeset.for_create(:create, %{title: "post", secret: "42", author_id: author.id})
      |> Ash.create!()

      all_access_posts_query = Ash.Query.for_read(Post, :all_access)

      assert [%{secret: "42"}] =
               author
               |> Ash.load!(posts: all_access_posts_query)
               |> Map.get(:posts)
    end

    test "unloading related data sets it back to `%Ash.NotLoaded{}`" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "zerg"})
        |> Ash.create!()

      post1 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post1"})
        |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
        |> Ash.create!()

      Post
      |> Ash.Changeset.for_create(:create, %{title: "post2"})
      |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
      |> Ash.create!()

      [author] =
        Author
        |> Ash.Query.load(posts: [:author])
        |> Ash.Query.filter(posts.id == ^post1.id)
        |> Ash.read!(authorize?: true)

      assert author
             |> Ash.Resource.unload([:posts, :author])
             |> Map.get(:posts)
             |> Enum.all?(fn post ->
               %Ash.NotLoaded{} = post.author
             end)
    end

    test "loading something does not unload previously loaded things" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "zerg"})
        |> Ash.create!()

      Post
      |> Ash.Changeset.for_create(:create, %{title: "post1"})
      |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
      |> Ash.create!()

      Post
      |> Ash.Changeset.for_create(:create, %{title: "post2"})
      |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
      |> Ash.create!()

      [author] =
        Author
        |> Ash.Query.load(:posts)
        |> Ash.read!(authorize?: true)
        |> Ash.load!(:latest_post)

      refute match?(%Ash.NotLoaded{}, author.posts)
      refute match?(%Ash.NotLoaded{}, author.latest_post)
    end

    test "you can load multiple relationships with `lazy?: true`" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "zerg"})
        |> Ash.create!()

      Post
      |> Ash.Changeset.for_create(:create, %{title: "post1"})
      |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
      |> Ash.create!()

      Post
      |> Ash.Changeset.for_create(:create, %{title: "post2"})
      |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
      |> Ash.create!()

      [author] =
        Author
        |> Ash.Query.load([:posts, :latest_post])
        |> Ash.read!(authorize?: true)

      Ash.load!(author, [:posts, :latest_post], lazy?: true)
    end

    test "you can load multiple depths of relationships with `lazy?: true` and sorts will work" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "zerg"})
        |> Ash.create!()

      Post
      |> Ash.Changeset.for_create(:create, %{title: "post1"})
      |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
      |> Ash.create!()

      Post
      |> Ash.Changeset.for_create(:create, %{title: "post2"})
      |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
      |> Ash.create!()

      [author] =
        Author
        |> Ash.Query.load(posts: :author)
        |> Ash.read!(authorize?: true)

      # doing this 25 times because its a regression test of an issue
      # with sorting that doesn't always go wrong
      for _ <- 0..25 do
        author =
          Map.update!(author, :posts, &Enum.shuffle/1)

        post_ids = Enum.map(author.posts, & &1.id)

        lazy_loaded_author =
          Ash.load!(author, [posts: [author: [posts: Ash.Query.sort(Post, title: :desc)]]],
            lazy?: true
          )

        assert Enum.map(lazy_loaded_author.posts, & &1.id) == post_ids

        assert Enum.all?(lazy_loaded_author.posts, fn post ->
                 titles = Enum.map(post.author.posts, & &1.title)

                 titles == Enum.sort(titles) |> Enum.reverse()
               end)
      end
    end

    @tag :regression
    test "you can lazy load through empty relationships without errors" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "zerg"})
        |> Ash.create!()

      Post
      |> Ash.Changeset.for_create(:create, %{title: "post1"})
      |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
      |> Ash.create!()

      Post
      |> Ash.Changeset.for_create(:create, %{title: "post2"})
      |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
      |> Ash.create!()

      Author
      |> Ash.Query.load([:posts])
      |> Ash.read!(authorize?: true)
      |> Ash.load!([posts: :author], lazy?: true)
    end

    test "loading something already loaded still loads it unless lazy?: true" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "zerg"})
        |> Ash.create!()

      post1 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post1"})
        |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
        |> Ash.create!()

      post2 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post2"})
        |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
        |> Ash.create!()

      [author] =
        Author
        |> Ash.Query.load(posts: [:author])
        |> Ash.Query.filter(posts.id == ^post1.id)
        |> Ash.read!(authorize?: true)

      author = Ash.load!(author, :posts_count, lazy?: true)

      assert author.posts_count == 2

      assert Enum.sort(Enum.map(author.posts, &Map.get(&1, :id))) ==
               Enum.sort([post1.id, post2.id])

      for post <- author.posts do
        assert post.author.id == author.id
      end

      post3 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post3"})
        |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
        |> Ash.create!()

      author =
        author
        |> Ash.load!([:posts_count, posts: [:author]], authorize?: true)

      assert author.posts_count == 3

      assert Enum.sort(Enum.map(author.posts, &Map.get(&1, :id))) ==
               Enum.sort([post1.id, post2.id, post3.id])

      for post <- author.posts do
        assert post.author.id == author.id
      end

      Post
      |> Ash.Changeset.for_create(:create, %{title: "post4"})
      |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
      |> Ash.create!()

      author =
        author
        |> Ash.load!([:posts_count, posts: [:author]], authorize?: true, lazy?: true)

      assert Enum.sort(Enum.map(author.posts, &Map.get(&1, :id))) ==
               Enum.sort([post1.id, post2.id, post3.id])

      assert author.posts_count == 3

      for post <- author.posts do
        assert post.author.id == author.id
      end

      post = Enum.at(author.posts, 0)
      post = Map.update!(post, :author, &Map.put(&1, :name, "shouldn't change"))

      assert post.author.name == "shouldn't change"

      author_after_load =
        post |> Ash.load!(:author, authorize?: false, lazy?: true) |> Map.get(:author)

      assert author_after_load.name == "shouldn't change"
    end

    test "it allows lazy-reloading calculations that have dependencies" do
      Campaign
      |> Ash.Changeset.for_create(:create, %{name: "hello World"})
      |> Ash.create!()

      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "zerg", campaign_name: "hello World"})
        |> Ash.create!()

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post1"})
        |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
        |> Ash.create!()

      post = Ash.load!(post, author: :campaign_upcase)
      author = post.author

      assert %{campaign_upcase: "HELLO WORLD"} = author

      assert %{campaign_upcase: "HELLO WORLD"} =
               Ash.load!(author, [:campaign_upcase], lazy?: true)
    end

    test "nested lazy loads work" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "zerg"})
        |> Ash.create!()

      post1 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post1"})
        |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
        |> Ash.create!()

      Post
      |> Ash.Changeset.for_create(:create, %{title: "post2"})
      |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
      |> Ash.create!()

      author =
        Author
        |> Ash.Query.load(:posts)
        |> Ash.Query.filter(posts.id == ^post1.id)
        |> Ash.read_one!(authorize?: true)
        |> Ash.load!([posts: :author], lazy?: true)

      author_id = author.id

      assert %{posts: [%{author: %{id: ^author_id}}, %{author: %{id: ^author_id}}]} = author

      post =
        Post
        |> Ash.Query.load(:author)
        |> Ash.Query.filter(id == ^post1.id)
        |> Ash.read_one!()
        |> Ash.load!([author: :posts], lazy?: true)

      assert %{author: %{posts: [_, _]}} = post
    end

    test "it allows loading across domains" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "zerg"})
        |> Ash.create!()

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post1"})
        |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
        |> Ash.create!()

      rating =
        Rating
        |> Ash.Changeset.for_create(:create, %{rating: 10})
        |> Ash.Changeset.manage_relationship(:post, post, type: :append_and_remove)
        |> Ash.create!()

      assert [loaded_rating] =
               author
               |> Ash.load!(posts: :ratings)
               |> Map.get(:posts)
               |> Enum.at(0)
               |> Map.get(:ratings)

      assert loaded_rating.id == rating.id
    end

    test "it allows loading many to many relationships" do
      category1 =
        Category
        |> Ash.Changeset.for_create(:create, %{name: "lame"})
        |> Ash.create!()

      category2 =
        Category
        |> Ash.Changeset.for_create(:create, %{name: "cool"})
        |> Ash.create!()

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post1"})
        |> Ash.Changeset.manage_relationship(:categories, [category1, category2],
          type: :append_and_remove
        )
        |> Ash.create!()

      [post] =
        Post
        |> Ash.Query.load(:categories)
        |> Ash.Query.filter(id == ^post.id)
        |> Ash.read!(authorize?: true)

      assert [%{id: id1}, %{id: id2}] = post.categories

      assert Enum.sort([category1.id, category2.id]) == Enum.sort([id1, id2])
    end

    test "ETS lateral join handles more records with matching source attribute" do
      category1 =
        Category
        |> Ash.Changeset.for_create(:create, %{name: "lame"})
        |> Ash.create!()

      Category
      |> Ash.Changeset.for_create(:create, %{name: "lame"})
      |> Ash.create!()

      category3 =
        Category
        |> Ash.Changeset.for_create(:create, %{name: "cool"})
        |> Ash.create!()

      LinkedCategory
      |> Ash.Changeset.for_create(:create, %{source_name: "lame", dest_name: "cool"})
      |> Ash.create!()

      linked_category_ids =
        category1
        |> Ash.load!(linked_categories: Ash.Query.limit(Category, 2))
        |> Map.get(:linked_categories)
        |> Enum.map(& &1.id)

      assert linked_category_ids == [category3.id]
    end

    test "it allows loading filtered many to many relationships with lateral joins" do
      category1 =
        Category
        |> Ash.Changeset.for_create(:create, %{name: "1"})
        |> Ash.create!()

      category2 =
        Category
        |> Ash.Changeset.for_create(:create, %{name: "2"})
        |> Ash.create!()

      category3 =
        Category
        |> Ash.Changeset.for_create(:create, %{name: "3"})
        |> Ash.create!()

      category4 =
        Category
        |> Ash.Changeset.for_create(:create, %{name: "4"})
        |> Ash.create!()

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post1"})
        |> Ash.Changeset.manage_relationship(
          :categories,
          [category1, category2, category3, category4],
          type: :append_and_remove
        )
        |> Ash.create!()

      category_query =
        Category
        |> Ash.Query.filter(name > "1")
        |> Ash.Query.sort(:name)
        |> Ash.Query.limit(2)

      [post] =
        Post
        |> Ash.Query.load(categories: category_query)
        |> Ash.Query.filter(id == ^post.id)
        |> Ash.read!(authorize?: true)

      assert [%{id: id1}, %{id: id2}] = post.categories

      assert [category2.id, category3.id] == [id1, id2]
    end

    test "it allows loading nested many to many relationships lazily" do
      category1 =
        Category
        |> Ash.Changeset.for_create(:create, %{name: "lame"})
        |> Ash.create!()

      category2 =
        Category
        |> Ash.Changeset.for_create(:create, %{name: "cool"})
        |> Ash.create!()

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post1"})
        |> Ash.Changeset.manage_relationship(:categories, [category1, category2],
          type: :append_and_remove
        )
        |> Ash.create!()

      assert [_] =
               Post
               |> Ash.Query.load(:categories)
               |> Ash.Query.filter(id == ^post.id)
               |> Ash.read!(authorize?: true)

      assert %{posts: [%{categories: [_, _]}]} =
               Category
               |> Ash.Query.filter(id == ^category1.id)
               |> Ash.Query.load(:posts)
               |> Ash.read_one!()
               |> Ash.load!([posts: :categories], lazy?: true)
    end

    test "it allows loading many to many relationships after the fact" do
      category1 =
        Category
        |> Ash.Changeset.for_create(:create, %{name: "lame"})
        |> Ash.create!()

      category2 =
        Category
        |> Ash.Changeset.for_create(:create, %{name: "cool"})
        |> Ash.create!()

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post1"})
        |> Ash.Changeset.manage_relationship(:categories, [category1, category2],
          type: :append_and_remove
        )
        |> Ash.create!()

      post = Post.get_by_id!(post.id, load: [:categories])

      assert [%{id: id1}, %{id: id2}] = post.categories

      assert Enum.sort([category1.id, category2.id]) == Enum.sort([id1, id2])
    end

    test "it produces a nice error message on loading invalid loads" do
      assert_raise Ash.Error.Invalid, ~r/:non_existent_thing is not a valid load/, fn ->
        Post
        |> Ash.Query.load(categories: [posts: :non_existent_thing])
        |> Ash.read!(authorize?: true)
      end
    end

    test "it allows loading nested many to many relationships" do
      category1 =
        Category
        |> Ash.Changeset.for_create(:create, %{name: "lame"})
        |> Ash.create!()

      category2 =
        Category
        |> Ash.Changeset.for_create(:create, %{name: "cool"})
        |> Ash.create!()

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post1"})
        |> Ash.Changeset.manage_relationship(:categories, [category1, category2],
          type: :append_and_remove
        )
        |> Ash.create!()

      [post] =
        Post
        |> Ash.Query.load(categories: :posts)
        |> Ash.Query.filter(id == ^post.id)
        |> Ash.read!(authorize?: true)

      post_id = post.id

      assert [%{posts: [%{id: ^post_id}]}, %{posts: [%{id: ^post_id}]}] = post.categories
    end

    test "it loads sorted relationships in the proper order" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "zerg"})
        |> Ash.create!()

      Post
      |> Ash.Changeset.for_create(:create, %{title: "post1"})
      |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
      |> Ash.create!()

      post2 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post2"})
        |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
        |> Ash.create!()

      [author] =
        Author
        |> Ash.Query.load(:latest_post)
        |> Ash.read!()

      assert author.latest_post.id == post2.id
    end

    test "it throws an error when passing it an Ash.Query" do
      assert_raise ArgumentError, ~r/Ash.load expects a record or records/, fn ->
        Post
        |> Ash.Query.for_read(:read)
        |> Ash.load!(:latest_post)
      end
    end

    test "loading belongs_to on resource without primary key returns correct result" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "test author"})
        |> Ash.create!()

      author2 =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "test author2"})
        |> Ash.create!()

      event_log =
        EventLog
        |> Ash.Changeset.for_create(:create, %{name: "test_event", author_id: author.id})
        |> Ash.create!()

      event_log2 =
        EventLog
        |> Ash.Changeset.for_create(:create, %{name: "test_event2", author_id: author2.id})
        |> Ash.create!()

      assert Ash.load!(event_log, :author).author.id == author.id
      assert Ash.load!(event_log2, :author).author.id == author2.id
    end
  end

  describe "forbidden lazy loads" do
    setup do
      start_supervised(
        {Ash.Test.Authorizer,
         strict_check: :authorized_if_actor,
         check: {:error, Ash.Error.Forbidden.exception([])},
         strict_check_context: [:query]}
      )

      :ok
    end

    test "action context is properly passed to the lazy query" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "zerg"})
        |> Ash.create!(authorize?: false)

      post1 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post1"})
        |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
        |> Ash.create!(authorize?: false)

      [author] =
        Author
        |> Ash.Query.load(:posts)
        |> Ash.Query.filter(posts.id == ^post1.id)
        |> Ash.read!(authorize?: false)

      assert {:ok, _} =
               Ash.load(author, [posts: [:author]], authorize?: true, lazy?: true, actor: author)
    end
  end

  test "it returns error with invalid keys" do
    post =
      Post
      |> Ash.Changeset.for_create(:create, %{title: "post1", category: "foo"})
      |> Ash.create!()

    assert_raise Ash.Error.Invalid, ~r/:invalid_key is not a valid load/, fn ->
      post |> Ash.load!([:invalid_key])
    end

    assert_raise Ash.Error.Invalid, ~r/:invalid_key is not a valid load/, fn ->
      Post.get_by_id!(post.id, load: [:invalid_key])
    end
  end

  describe "loading through attributes" do
    setup do
      start_supervised(
        {Ash.Test.Authorizer,
         strict_check: :authorized,
         check: {:error, Ash.Error.Forbidden.exception([])},
         strict_check_context: [:query]}
      )

      :ok
    end

    test "can load calculations through attributes" do
      Author
      |> Ash.Changeset.for_create(
        :create,
        %{name: "zerg", bio: %{first_name: "donald", last_name: "duck"}},
        authorize?: false
      )
      |> Ash.create!()

      assert [%{bio: %{full_name: "donald duck"}}] =
               Author
               |> Ash.Query.load(bio: :full_name)
               |> Ash.read!()
    end

    test "can load calculations through nil attributes" do
      Author
      |> Ash.Changeset.for_create(:create, %{name: "zerg"})
      |> Ash.create!()

      assert [%{bio: nil}] =
               Author
               |> Ash.Query.load(bio: :full_name)
               |> Ash.read!()
    end

    test "can load calculations through union" do
      Author
      |> Ash.Changeset.for_create(
        :create,
        %{name: "zerg", bio_union: %{type: "bio", first_name: "donald", last_name: "duck"}},
        authorize?: false
      )
      |> Ash.create!(authorize?: false)

      Author
      |> Ash.Changeset.for_create(
        :create,
        %{
          name: "zerg",
          bio_union: %{type: "other_kind_of_bio", first_name: "donald", last_name: "duck"}
        },
        authorize?: false
      )
      |> Ash.create!()

      assert [
               %{bio_union: %Ash.Union{value: %{full_name: "donald duck"}}},
               %{bio_union: %Ash.Union{value: %{full_name: "donald duck"}}}
             ] =
               Author
               |> Ash.Query.load(bio_union: [*: :full_name])
               |> Ash.read!()

      assert [
               %{bio_union: %Ash.Union{value: %{full_name: "donald duck"}}},
               %{bio_union: %Ash.Union{value: %{full_name: "donald duck"}}}
             ] =
               Author
               |> Ash.Query.load(bio_union: [bio: :full_name, other_kind_of_bio: :full_name])
               |> Ash.read!()
    end

    test "can load calculations through union produced by a calculation" do
      Author
      |> Ash.Changeset.for_create(
        :create,
        %{
          name: "zerg",
          bio_union: %{type: "bio", first_name: "donald", last_name: "duck"}
        },
        authorize?: false
      )
      |> Ash.create!()

      Author
      |> Ash.Changeset.for_create(
        :create,
        %{
          name: "zerg",
          bio_union: %{type: "other_kind_of_bio", first_name: "donald", last_name: "duck"}
        },
        authorize?: false
      )
      |> Ash.create!()

      assert [
               %{
                 bio_union_calc: %Ash.Union{
                   value: %{
                     full_name: "donald duck",
                     forbidden_name: %Ash.ForbiddenField{},
                     forbidden_field: %Ash.ForbiddenField{}
                   }
                 }
               },
               %{
                 bio_union_calc: %Ash.Union{
                   value: %{
                     full_name: "donald duck",
                     forbidden_name: %Ash.ForbiddenField{},
                     forbidden_field: %Ash.ForbiddenField{}
                   }
                 }
               }
             ] =
               Author
               |> Ash.Query.load(bio_union_calc: {%{}, [*: [:full_name, :forbidden_name]]})
               |> Ash.read!(actor: %{name: "zerg"}, authorize?: true)
    end
  end

  describe "relationship pagination" do
    setup do
      start_supervised(
        {Ash.Test.Authorizer,
         strict_check: :authorized,
         check: {:error, Ash.Error.Forbidden.exception([])},
         strict_check_context: [:query]}
      )

      :ok
    end

    test "it allows paginating has_many relationships with offset pagination" do
      author1 =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "a"})
        |> Ash.create!()

      author2 =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "b"})
        |> Ash.create!()

      for i <- 0..9 do
        Post
        |> Ash.Changeset.for_create(:create, %{title: "author1 post#{i}", author_id: author1.id})
        |> Ash.create!()

        Post
        |> Ash.Changeset.for_create(:create, %{title: "author2 post#{i}", author_id: author2.id})
        |> Ash.create!()
      end

      paginated_posts =
        Post
        |> Ash.Query.page(limit: 2, offset: 2)
        |> Ash.Query.sort(:title)

      assert [author1, author2] =
               Author
               |> Ash.Query.sort(:name)
               |> Ash.Query.load(posts: paginated_posts)
               |> Ash.read!()

      assert %Ash.Page.Offset{
               results: [%{title: "author1 post2"}, %{title: "author1 post3"}]
             } = author1.posts

      assert %Ash.Page.Offset{
               results: [%{title: "author2 post2"}, %{title: "author2 post3"}]
             } = author2.posts

      assert %Ash.Page.Offset{
               results: [%{title: "author1 post4"}, %{title: "author1 post5"}]
             } = Ash.page!(author1.posts, :next)
    end

    test "it allows paginating has_many relationships with keyset pagination" do
      author1 =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "a"})
        |> Ash.create!()

      author2 =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "b"})
        |> Ash.create!()

      for i <- 0..9 do
        Post
        |> Ash.Changeset.for_create(:create, %{title: "author1 post#{i}", author_id: author1.id})
        |> Ash.create!()

        Post
        |> Ash.Changeset.for_create(:create, %{title: "author2 post#{i}", author_id: author2.id})
        |> Ash.create!()
      end

      paginated_posts =
        Post
        |> Ash.Query.for_read(:keyset)
        |> Ash.Query.page(limit: 2)
        |> Ash.Query.sort(:title)

      assert [author1, author2] =
               Author
               |> Ash.Query.sort(:name)
               |> Ash.Query.load(posts: paginated_posts)
               |> Ash.read!()

      assert %Ash.Page.Keyset{
               results: [%{title: "author1 post0"}, %{title: "author1 post1"}]
             } = author1.posts

      assert %Ash.Page.Keyset{
               results: [%{title: "author2 post0"}, %{title: "author2 post1"}]
             } = author2.posts

      assert %Ash.Page.Keyset{
               results: [%{title: "author1 post2"}, %{title: "author1 post3"}]
             } = Ash.page!(author1.posts, :next)
    end

    test "it allows paginating many_to_many relationships with offset pagination" do
      categories =
        for i <- 0..9 do
          Category
          |> Ash.Changeset.for_create(:create, %{name: "category#{i}"})
          |> Ash.create!()
        end

      categories_0_to_6 = Enum.take(categories, 6)
      categories_5_to_9 = Enum.slice(categories, 5..9)

      Post
      |> Ash.Changeset.for_create(:create, %{title: "a"})
      |> Ash.Changeset.manage_relationship(:categories, categories_0_to_6,
        type: :append_and_remove
      )
      |> Ash.create!()

      Post
      |> Ash.Changeset.for_create(:create, %{title: "b"})
      |> Ash.Changeset.manage_relationship(:categories, categories_5_to_9,
        type: :append_and_remove
      )
      |> Ash.create!()

      paginated_categories =
        Category
        |> Ash.Query.page(limit: 2, offset: 2)
        |> Ash.Query.sort(:name)

      assert [post1, post2] =
               Post
               |> Ash.Query.sort(:title)
               |> Ash.Query.load(categories: paginated_categories)
               |> Ash.read!()

      assert %Ash.Page.Offset{
               results: [%{name: "category2"}, %{name: "category3"}]
             } = post1.categories

      assert %Ash.Page.Offset{
               results: [%{name: "category7"}, %{name: "category8"}]
             } = post2.categories

      assert %Ash.Page.Offset{
               results: [%{name: "category4"}, %{name: "category5"}]
             } = Ash.page!(post1.categories, :next)
    end

    test "it allows paginating many_to_many relationships with keyset pagination" do
      categories =
        for i <- 0..9 do
          Category
          |> Ash.Changeset.for_create(:create, %{name: "category#{i}"})
          |> Ash.create!()
        end

      categories_0_to_6 = Enum.take(categories, 6)
      categories_5_to_9 = Enum.slice(categories, 5..9)

      Post
      |> Ash.Changeset.for_create(:create, %{title: "a"})
      |> Ash.Changeset.manage_relationship(:categories, categories_0_to_6,
        type: :append_and_remove
      )
      |> Ash.create!()

      Post
      |> Ash.Changeset.for_create(:create, %{title: "b"})
      |> Ash.Changeset.manage_relationship(:categories, categories_5_to_9,
        type: :append_and_remove
      )
      |> Ash.create!()

      paginated_categories =
        Category
        |> Ash.Query.for_read(:keyset)
        |> Ash.Query.page(limit: 2)
        |> Ash.Query.sort(:name)

      assert [post1, post2] =
               Post
               |> Ash.Query.sort(:title)
               |> Ash.Query.load(categories: paginated_categories)
               |> Ash.read!()

      assert %Ash.Page.Keyset{
               results: [%{name: "category0"}, %{name: "category1"}]
             } = post1.categories

      assert %Ash.Page.Keyset{
               results: [%{name: "category5"}, %{name: "category6"}]
             } = post2.categories

      assert %Ash.Page.Keyset{
               results: [%{name: "category2"}, %{name: "category3"}]
             } = Ash.page!(post1.categories, :next)
    end

    test "works when nested with offset" do
      author1 =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "a"})
        |> Ash.create!()

      author2 =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "b"})
        |> Ash.create!()

      categories =
        for i <- 0..9 do
          Category
          |> Ash.Changeset.for_create(:create, %{name: "category#{i}"})
          |> Ash.create!()
        end

      categories_0_to_6 = Enum.take(categories, 6)
      categories_5_to_9 = Enum.slice(categories, 5..9)

      for i <- 0..5 do
        Post
        |> Ash.Changeset.for_create(:create, %{title: "author1 post#{i}", author_id: author1.id})
        |> Ash.Changeset.manage_relationship(:categories, categories_0_to_6,
          type: :append_and_remove
        )
        |> Ash.create!()

        Post
        |> Ash.Changeset.for_create(:create, %{title: "author2 post#{i}", author_id: author2.id})
        |> Ash.Changeset.manage_relationship(:categories, categories_5_to_9,
          type: :append_and_remove
        )
        |> Ash.create!()
      end

      paginated_categories =
        Category
        |> Ash.Query.page(limit: 1, offset: 2)
        |> Ash.Query.sort(:name)

      paginated_posts =
        Post
        |> Ash.Query.load(categories: paginated_categories)
        |> Ash.Query.page(limit: 1, offset: 1)
        |> Ash.Query.sort(:title)

      assert [author1, _author2] =
               Author
               |> Ash.Query.sort(:name)
               |> Ash.Query.load(posts: paginated_posts)
               |> Ash.read!()

      assert %Ash.Page.Offset{
               results: [
                 %{
                   title: "author1 post1",
                   categories: %Ash.Page.Offset{results: [%{name: "category2"}]}
                 }
               ]
             } = author1.posts
    end

    test "works when nested with keyset" do
      author1 =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "a"})
        |> Ash.create!()

      author2 =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "b"})
        |> Ash.create!()

      categories =
        for i <- 0..9 do
          Category
          |> Ash.Changeset.for_create(:create, %{name: "category#{i}"})
          |> Ash.create!()
        end

      categories_0_to_6 = Enum.take(categories, 6)
      categories_5_to_9 = Enum.slice(categories, 5..9)

      for i <- 0..5 do
        Post
        |> Ash.Changeset.for_create(:create, %{title: "author1 post#{i}", author_id: author1.id})
        |> Ash.Changeset.manage_relationship(:categories, categories_0_to_6,
          type: :append_and_remove
        )
        |> Ash.create!()

        Post
        |> Ash.Changeset.for_create(:create, %{title: "author2 post#{i}", author_id: author2.id})
        |> Ash.Changeset.manage_relationship(:categories, categories_5_to_9,
          type: :append_and_remove
        )
        |> Ash.create!()
      end

      paginated_categories =
        Category
        |> Ash.Query.for_read(:keyset)
        |> Ash.Query.page(limit: 1)
        |> Ash.Query.sort(:name)

      paginated_posts =
        Post
        |> Ash.Query.for_read(:keyset)
        |> Ash.Query.load(categories: paginated_categories)
        |> Ash.Query.page(limit: 1)
        |> Ash.Query.sort(:title)

      assert [author1, _author2] =
               Author
               |> Ash.Query.sort(:name)
               |> Ash.Query.load(posts: paginated_posts)
               |> Ash.read!()

      assert %Ash.Page.Keyset{
               results: [
                 %{
                   title: "author1 post0",
                   categories: %Ash.Page.Keyset{results: [%{name: "category0"}]}
                 }
               ]
             } = author1.posts
    end

    test "doesn't honor required? pagination to maintain backwards compatibility" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "a"})
        |> Ash.create!()

      Post
      |> Ash.Changeset.for_create(:create, %{title: "b"})
      |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
      |> Ash.create!()

      posts =
        Post
        |> Ash.Query.for_read(:required_pagination)

      assert [_post] =
               Author
               |> Ash.read!()
               |> Ash.load!(posts: posts)
    end

    test "it allows counting has_many relationships" do
      author1 =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "a"})
        |> Ash.create!()

      author2 =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "b"})
        |> Ash.create!()

      for i <- 1..3 do
        Post
        |> Ash.Changeset.for_create(:create, %{title: "author1 post#{i}", author_id: author1.id})
        |> Ash.create!()
      end

      for i <- 1..6 do
        Post
        |> Ash.Changeset.for_create(:create, %{title: "author2 post#{i}", author_id: author2.id})
        |> Ash.create!()
      end

      paginated_posts =
        Post
        |> Ash.Query.page(limit: 2, offset: 2, count: true)

      assert [author1, author2] =
               Author
               |> Ash.Query.sort(:name)
               |> Ash.Query.load(posts: paginated_posts)
               |> Ash.read!()

      assert %Ash.Page.Offset{count: 3} = author1.posts
      assert %Ash.Page.Offset{count: 6} = author2.posts
    end

    test "it allows counting many_to_many relationships" do
      categories =
        for i <- 1..9 do
          Category
          |> Ash.Changeset.for_create(:create, %{name: "category#{i}"})
          |> Ash.create!()
        end

      categories_1_to_3 = Enum.take(categories, 3)
      categories_4_to_9 = Enum.slice(categories, 3..9)

      Post
      |> Ash.Changeset.for_create(:create, %{title: "a"})
      |> Ash.Changeset.manage_relationship(:categories, categories_1_to_3,
        type: :append_and_remove
      )
      |> Ash.create!()

      Post
      |> Ash.Changeset.for_create(:create, %{title: "b"})
      |> Ash.Changeset.manage_relationship(:categories, categories_4_to_9,
        type: :append_and_remove
      )
      |> Ash.create!()

      paginated_categories =
        Category
        |> Ash.Query.page(limit: 2, count: true)
        |> Ash.Query.sort(:name)

      assert [post1, post2] =
               Post
               |> Ash.Query.sort(:title)
               |> Ash.Query.load(categories: paginated_categories)
               |> Ash.read!()

      assert %Ash.Page.Keyset{count: 3} = post1.categories
      assert %Ash.Page.Keyset{count: 6} = post2.categories
    end

    test "allows counting nested relationships" do
      author1 =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "a"})
        |> Ash.create!()

      _author2 =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "b"})
        |> Ash.create!()

      categories =
        for i <- 1..3 do
          Category
          |> Ash.Changeset.for_create(:create, %{name: "category#{i}"})
          |> Ash.create!()
        end

      for i <- 1..5 do
        Post
        |> Ash.Changeset.for_create(:create, %{title: "author1 post#{i}", author_id: author1.id})
        |> Ash.Changeset.manage_relationship(:categories, categories, type: :append_and_remove)
        |> Ash.create!()
      end

      paginated_categories =
        Category
        |> Ash.Query.page(limit: 1, count: true)

      paginated_posts =
        Post
        |> Ash.Query.load(categories: paginated_categories)
        |> Ash.Query.page(limit: 1, count: true)

      assert %Ash.Page.Keyset{results: [author1], count: 2} =
               Author
               |> Ash.Query.sort(:name)
               |> Ash.Query.load(posts: paginated_posts)
               |> Ash.read!(page: [limit: 1, count: true])

      assert %Ash.Page.Keyset{count: 5, results: [%{categories: %Ash.Page.Keyset{count: 3}}]} =
               author1.posts
    end

    test "doesn't leak the internal count aggregate when counting" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "a"})
        |> Ash.create!()

      for i <- 1..3 do
        Post
        |> Ash.Changeset.for_create(:create, %{title: "author1 post#{i}", author_id: author.id})
        |> Ash.create!()
      end

      paginated_posts =
        Post
        |> Ash.Query.page(limit: 2, offset: 2, count: true)

      assert [author] =
               Author
               |> Ash.Query.load(posts: paginated_posts)
               |> Ash.read!()

      assert %Ash.Page.Offset{count: 3} = author.posts
      assert %{} == author.aggregates
    end

    test "strict?: true option only loads specified fields on related resource" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "a"})
        |> Ash.create!()

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{
          title: "author post",
          contents: "post content",
          author_id: author.id
        })
        |> Ash.create!()

      assert [author] =
               Author
               |> Ash.Query.load([posts: [:title]], strict?: true)
               |> Ash.read!()

      [loaded_post] = author.posts

      assert post.contents == "post content"
      assert loaded_post.title == "author post"
      assert %Ash.NotLoaded{} = loaded_post.contents
    end

    test "all fields are loaded if no fields are specified when using strict?: true" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "a"})
        |> Ash.create!()

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{
          title: "author post",
          contents: "post content",
          author_id: author.id
        })
        |> Ash.create!()

      assert [author] =
               Author
               |> Ash.Query.load([:posts], strict?: true)
               |> Ash.read!()

      [loaded_post] = author.posts

      assert post.title == loaded_post.title
      assert post.contents == loaded_post.contents
    end

    test "strict?: true option only loads specified fields on related resource for Ash.load" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "a"})
        |> Ash.create!()

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{
          title: "author post",
          contents: "post content",
          author_id: author.id
        })
        |> Ash.create!()

      loaded_author = Ash.load!(author, [posts: [:title]], strict?: true)
      [loaded_post] = loaded_author.posts

      assert post.contents == "post content"
      assert loaded_post.title == "author post"
      assert %Ash.NotLoaded{} = loaded_post.contents
    end

    test "all fields are loaded if no fields are specified when using strict?: true for Ash.load" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "a"})
        |> Ash.create!()

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{
          title: "author post",
          contents: "post content",
          author_id: author.id
        })
        |> Ash.create!()

      loaded_author = Ash.load!(author, [:posts], strict?: true)
      [loaded_post] = loaded_author.posts

      assert post.title == loaded_post.title
      assert post.contents == loaded_post.contents
    end
  end

  test "read action loads are ignored with `Ash.load`" do
    author =
      Author
      |> Ash.Changeset.for_create(:create, %{name: "a"})
      |> Ash.create!()

    post =
      Post
      |> Ash.Changeset.for_create(:create, %{
        title: "author post",
        contents: "post content",
        author_id: author.id
      })
      |> Ash.create!()

    assert %Ash.NotLoaded{} = Ash.load!(post, :author).category_length
  end

  test "read action loads of nested relationships are respected with `Ash.load`" do
    author =
      Author
      |> Ash.Changeset.for_create(:create, %{name: "a"})
      |> Ash.create!()

    _post =
      Post
      |> Ash.Changeset.for_create(:create, %{
        title: "author post",
        contents: "post content",
        category: "category 1",
        author_id: author.id
      })
      |> Ash.create!()

    %{posts: [post]} = Ash.load!(author, [:posts])
    assert "10" == post.category_length
  end

  test "calculation arguments should be respected for root loads" do
    post = Post |> Ash.Changeset.for_create(:create, %{}) |> Ash.create!()

    post = post |> Ash.load!(sum: [])
    assert 3 == post.sum

    post = post |> Ash.load!(sum: [a: 3, b: 4])
    assert 7 == post.sum
  end

  test "calculation arguments should be respected for nested loads" do
    author = Author |> Ash.Changeset.for_create(:create, %{}) |> Ash.create!()
    Post |> Ash.Changeset.for_create(:create, %{author_id: author.id}) |> Ash.create!()

    %{posts: [post]} = author |> Ash.load!(posts: [sum: []])
    assert 3 = post.sum

    %{posts: [post]} = author |> Ash.load!(posts: [sum: [a: 3, b: 4]])
    assert 7 == post.sum
  end

  test "you can load data on create with no default read action" do
    record =
      ExampleWithNoPrimaryRead
      |> Ash.Changeset.for_create(:create, %{})
      |> Ash.Changeset.load(:id_calc)
      |> Ash.create!()

    assert record.id == record.id_calc
  end
end
