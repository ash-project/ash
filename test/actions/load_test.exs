defmodule Ash.Test.Actions.LoadTest do
  @moduledoc false
  use ExUnit.Case, async: false

  require Ash.Query

  alias Ash.Test.Domain, as: Domain

  defmodule Campaign do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      defaults([:create, :read, :update, :destroy])
    end

    attributes do
      uuid_primary_key(:id)
      attribute(:name, :ci_string)
    end
  end

  defmodule Bio do
    use Ash.Resource,
      authorizers: [Ash.Policy.Authorizer],
      data_layer: :embedded

    attributes do
      attribute(:first_name, :string)
      attribute(:last_name, :string)

      attribute :type, :string do
        allow_nil?(false)
        default("bio")
        private?(true)
      end

      attribute :forbidden_field, :string
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
      calculate(:full_name, :string, expr(first_name <> " " <> last_name))
      calculate(:forbidden_name, :string, expr(first_name <> " " <> last_name))
    end
  end

  defmodule OtherKindOfBio do
    use Ash.Resource,
      authorizers: [Ash.Policy.Authorizer],
      data_layer: :embedded

    attributes do
      attribute(:first_name, :string)
      attribute(:last_name, :string)

      attribute :type, :string do
        allow_nil?(false)
        default("other_kind_of_bio")
        private?(true)
      end

      attribute :forbidden_field, :string
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
      calculate(:full_name, :string, expr(first_name <> " " <> last_name))

      calculate(:forbidden_name, :string, expr(first_name <> " " <> last_name))
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
            cast_tag?: false,
            tag_value: "bio"
          ],
          other_kind_of_bio: [
            type: OtherKindOfBio,
            cast_tag?: false,
            tag: :type,
            tag_value: "other_kind_of_bio"
          ]
        ]
      ]
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
      defaults([:create, :read, :update, :destroy])
    end

    attributes do
      uuid_primary_key(:id)
      attribute(:name, :string)
      attribute(:bio, Bio)
      attribute(:bio_union, BioUnion)
    end

    calculations do
      calculate(:bio_union_calc, BioUnion, expr(bio_union))
    end

    relationships do
      has_many(:posts, Ash.Test.Actions.LoadTest.Post, destination_attribute: :author_id)

      has_one(:latest_post, Ash.Test.Actions.LoadTest.Post,
        destination_attribute: :author_id,
        sort: [inserted_at: :desc]
      )

      belongs_to :campaign, Ash.Test.Actions.LoadTest.Campaign do
        attribute_type(:ci_string)
        source_attribute(:campaign_name)
        destination_attribute(:name)
        attribute_writable?(true)
      end
    end
  end

  defmodule PostsInSameCategory do
    use Ash.Resource.ManualRelationship

    def load(posts, _, %{query: destination_query, domain: domain}) do
      categories = Enum.map(posts, & &1.category)

      other_posts =
        destination_query
        |> Ash.Query.filter(category in ^categories)
        |> domain.read!()
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
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      defaults([:create, :read, :update, :destroy])
    end

    attributes do
      uuid_primary_key(:id)
      attribute(:title, :string)
      attribute(:contents, :string)
      attribute(:category, :string)
      timestamps()
    end

    code_interface do
      define :get_by_id do
        action(:read)
        get_by([:id])
      end
    end

    relationships do
      belongs_to :author, Author do
        attribute_writable? true
      end

      has_many :posts_in_same_category, __MODULE__ do
        manual(PostsInSameCategory)
      end

      has_many :ratings, Ash.Test.Actions.LoadTest.Rating do
        domain(Ash.Test.Actions.LoadTest.Domain2)
      end

      has_many :posts_with_same_title, __MODULE__ do
        no_attributes? true
        filter expr(parent(title) == title and parent(id) != id)
      end

      many_to_many(:categories, Ash.Test.Actions.LoadTest.Category,
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
      defaults([:create, :read, :update, :destroy])
    end

    relationships do
      belongs_to(:post, Post, primary_key?: true, allow_nil?: false)

      belongs_to(:category, Ash.Test.Actions.LoadTest.Category,
        primary_key?: true,
        allow_nil?: false
      )
    end
  end

  defmodule Category do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      defaults([:create, :read, :update, :destroy])
    end

    attributes do
      uuid_primary_key(:id)
      attribute(:name, :string)
    end

    relationships do
      many_to_many(:posts, Post,
        through: PostCategory,
        destination_attribute_on_join_resource: :post_id,
        source_attribute_on_join_resource: :category_id
      )
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
      attribute(:rating, :integer)
    end

    actions do
      defaults([:create, :read, :update, :destroy])
    end

    relationships do
      belongs_to :post, Post do
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

  setup do
    start_supervised(
      {Ash.Test.Authorizer,
       strict_check: :authorized,
       check: {:error, Ash.Error.Forbidden.exception([])},
       strict_check_context: [:query]}
    )

    :ok
  end

  describe "loads" do
    test "it allows loading manual relationships" do
      post1 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post1", category: "foo"})
        |> Domain.create!()

      Post
      |> Ash.Changeset.for_create(:create, %{title: "post2", category: "bar"})
      |> Domain.create!()

      post3 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post2", category: "foo"})
        |> Domain.create!()

      post3_id = post3.id

      assert [%{id: ^post3_id}] =
               post1
               |> Domain.load!(:posts_in_same_category)
               |> Map.get(:posts_in_same_category)
    end

    test "parent expressions can be used for complex constraints" do
      post1 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post1", category: "foo"})
        |> Domain.create!()

      post1_same =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post1", category: "bar"})
        |> Domain.create!()

      Post
      |> Ash.Changeset.for_create(:create, %{title: "post2", category: "baz"})
      |> Domain.create!()

      post1_same_id = post1_same.id

      assert [%{id: ^post1_same_id}] =
               post1
               |> Domain.load!(:posts_with_same_title)
               |> Map.get(:posts_with_same_title)
    end

    test "it allows loading through manual relationships" do
      post1 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post1", category: "foo"})
        |> Domain.create!()

      Post
      |> Ash.Changeset.for_create(:create, %{title: "post2", category: "bar"})
      |> Domain.create!()

      post3 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post2", category: "foo"})
        |> Domain.create!()

      post3_id = post3.id

      assert [%{id: ^post3_id}] =
               post1
               |> Domain.load!(posts_in_same_category: :ratings)
               |> Map.get(:posts_in_same_category)
    end

    test "it uses `Comp.equal?/2` to support things like ci_string foreign keys" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "zerg", campaign_name: "FrEd"})
        |> Domain.create!()

      Campaign
      |> Ash.Changeset.for_create(:create, %{name: "fReD"})
      |> Domain.create!()

      assert %{
               campaign: %{name: %Ash.CiString{string: "fReD"}},
               campaign_name: %Ash.CiString{string: "FrEd"}
             } = Domain.load!(author, :campaign)
    end

    test "it allows loading data" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "zerg"})
        |> Domain.create!()

      post1 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post1", author_id: author.id})
        |> Domain.create!()

      post2 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post2", author_id: author.id})
        |> Domain.create!()

      assert [fetched_post1, fetched_post2] =
               author
               |> Domain.load!(:posts)
               |> Map.get(:posts)

      assert Enum.sort([post1.id, post2.id]) == Enum.sort([fetched_post1.id, fetched_post2.id])
    end

    test "it allows loading nested related data" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "zerg"})
        |> Domain.create!()

      post1 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post1", author_id: author.id})
        |> Domain.create!()

      post2 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post2", author_id: author.id})
        |> Domain.create!()

      [author] =
        Author
        |> Ash.Query.load(posts: [:author])
        |> Ash.Query.filter(posts.id == ^post1.id)
        |> Domain.read!(authorize?: true)

      assert Enum.sort(Enum.map(author.posts, &Map.get(&1, :id))) ==
               Enum.sort([post1.id, post2.id])

      for post <- author.posts do
        assert post.author.id == author.id
      end
    end

    test "unloading related data sets it back to `%Ash.NotLoaded{}`" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "zerg"})
        |> Domain.create!()

      post1 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post1"})
        |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
        |> Domain.create!()

      Post
      |> Ash.Changeset.for_create(:create, %{title: "post2"})
      |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
      |> Domain.create!()

      [author] =
        Author
        |> Ash.Query.load(posts: [:author])
        |> Ash.Query.filter(posts.id == ^post1.id)
        |> Domain.read!(authorize?: true)

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
        |> Domain.create!()

      Post
      |> Ash.Changeset.for_create(:create, %{title: "post1"})
      |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
      |> Domain.create!()

      Post
      |> Ash.Changeset.for_create(:create, %{title: "post2"})
      |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
      |> Domain.create!()

      [author] =
        Author
        |> Ash.Query.load(:posts)
        |> Domain.read!(authorize?: true)
        |> Domain.load!(:latest_post)

      refute match?(%Ash.NotLoaded{}, author.posts)
      refute match?(%Ash.NotLoaded{}, author.latest_post)
    end

    test "loading something already loaded still loads it unless lazy?: true" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "zerg"})
        |> Domain.create!()

      post1 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post1"})
        |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
        |> Domain.create!()

      post2 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post2"})
        |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
        |> Domain.create!()

      [author] =
        Author
        |> Ash.Query.load(posts: [:author])
        |> Ash.Query.filter(posts.id == ^post1.id)
        |> Domain.read!(authorize?: true)

      assert Enum.sort(Enum.map(author.posts, &Map.get(&1, :id))) ==
               Enum.sort([post1.id, post2.id])

      for post <- author.posts do
        assert post.author.id == author.id
      end

      post3 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post3"})
        |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
        |> Domain.create!()

      author =
        author
        |> Domain.load!([posts: [:author]], authorize?: true)

      assert Enum.sort(Enum.map(author.posts, &Map.get(&1, :id))) ==
               Enum.sort([post1.id, post2.id, post3.id])

      for post <- author.posts do
        assert post.author.id == author.id
      end

      Post
      |> Ash.Changeset.for_create(:create, %{title: "post4"})
      |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
      |> Domain.create!()

      author =
        author
        |> Domain.load!([posts: [:author]], authorize?: true, lazy?: true)

      assert Enum.sort(Enum.map(author.posts, &Map.get(&1, :id))) ==
               Enum.sort([post1.id, post2.id, post3.id])

      for post <- author.posts do
        assert post.author.id == author.id
      end
    end

    test "nested lazy loads work" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "zerg"})
        |> Domain.create!()

      post1 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post1"})
        |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
        |> Domain.create!()

      Post
      |> Ash.Changeset.for_create(:create, %{title: "post2"})
      |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
      |> Domain.create!()

      author =
        Author
        |> Ash.Query.load(:posts)
        |> Ash.Query.filter(posts.id == ^post1.id)
        |> Domain.read_one!(authorize?: true)
        |> Domain.load!([posts: :author], lazy?: true)

      author_id = author.id

      assert %{posts: [%{author: %{id: ^author_id}}, %{author: %{id: ^author_id}}]} = author

      post =
        Post
        |> Ash.Query.load(:author)
        |> Ash.Query.filter(id == ^post1.id)
        |> Domain.read_one!()
        |> Domain.load!([author: :posts], lazy?: true)

      assert %{author: %{posts: [_, _]}} = post
    end

    test "it allows loading across domains" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "zerg"})
        |> Domain.create!()

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post1"})
        |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
        |> Domain.create!()

      rating =
        Rating
        |> Ash.Changeset.for_create(:create, %{rating: 10})
        |> Ash.Changeset.manage_relationship(:post, post, type: :append_and_remove)
        |> Domain2.create!()

      assert [loaded_rating] =
               author
               |> Domain.load!(posts: :ratings)
               |> Map.get(:posts)
               |> Enum.at(0)
               |> Map.get(:ratings)

      assert loaded_rating.id == rating.id
    end

    test "it allows loading many to many relationships" do
      category1 =
        Category
        |> Ash.Changeset.for_create(:create, %{name: "lame"})
        |> Domain.create!()

      category2 =
        Category
        |> Ash.Changeset.for_create(:create, %{name: "cool"})
        |> Domain.create!()

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post1"})
        |> Ash.Changeset.manage_relationship(:categories, [category1, category2],
          type: :append_and_remove
        )
        |> Domain.create!()

      [post] =
        Post
        |> Ash.Query.load(:categories)
        |> Ash.Query.filter(id == ^post.id)
        |> Domain.read!(authorize?: true)

      assert [%{id: id1}, %{id: id2}] = post.categories

      assert Enum.sort([category1.id, category2.id]) == Enum.sort([id1, id2])
    end

    test "it allows loading nested many to many relationships lazily" do
      category1 =
        Category
        |> Ash.Changeset.for_create(:create, %{name: "lame"})
        |> Domain.create!()

      category2 =
        Category
        |> Ash.Changeset.for_create(:create, %{name: "cool"})
        |> Domain.create!()

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post1"})
        |> Ash.Changeset.manage_relationship(:categories, [category1, category2],
          type: :append_and_remove
        )
        |> Domain.create!()

      assert [_] =
               Post
               |> Ash.Query.load(:categories)
               |> Ash.Query.filter(id == ^post.id)
               |> Domain.read!(authorize?: true)

      assert %{posts: [%{categories: [_, _]}]} =
               Category
               |> Ash.Query.filter(id == ^category1.id)
               |> Ash.Query.load(:posts)
               |> Domain.read_one!()
               |> Domain.load!([posts: :categories], lazy?: true)
    end

    test "it allows loading many to many relationships after the fact" do
      category1 =
        Category
        |> Ash.Changeset.for_create(:create, %{name: "lame"})
        |> Domain.create!()

      category2 =
        Category
        |> Ash.Changeset.for_create(:create, %{name: "cool"})
        |> Domain.create!()

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post1"})
        |> Ash.Changeset.manage_relationship(:categories, [category1, category2],
          type: :append_and_remove
        )
        |> Domain.create!()

      post = Post.get_by_id!(post.id, load: [:categories])

      assert [%{id: id1}, %{id: id2}] = post.categories

      assert Enum.sort([category1.id, category2.id]) == Enum.sort([id1, id2])
    end

    test "it produces a nice error message on loading invalid loads" do
      assert_raise Ash.Error.Invalid, ~r/:non_existent_thing is not a valid load/, fn ->
        Post
        |> Ash.Query.load(categories: [posts: :non_existent_thing])
        |> Domain.read!(authorize?: true)
      end
    end

    test "it allows loading nested many to many relationships" do
      category1 =
        Category
        |> Ash.Changeset.for_create(:create, %{name: "lame"})
        |> Domain.create!()

      category2 =
        Category
        |> Ash.Changeset.for_create(:create, %{name: "cool"})
        |> Domain.create!()

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post1"})
        |> Ash.Changeset.manage_relationship(:categories, [category1, category2],
          type: :append_and_remove
        )
        |> Domain.create!()

      [post] =
        Post
        |> Ash.Query.load(categories: :posts)
        |> Ash.Query.filter(id == ^post.id)
        |> Domain.read!(authorize?: true)

      post_id = post.id

      assert [%{posts: [%{id: ^post_id}]}, %{posts: [%{id: ^post_id}]}] = post.categories
    end

    test "it loads sorted relationships in the proper order" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "zerg"})
        |> Domain.create!()

      Post
      |> Ash.Changeset.for_create(:create, %{title: "post1"})
      |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
      |> Domain.create!()

      post2 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post2"})
        |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
        |> Domain.create!()

      [author] =
        Author
        |> Ash.Query.load(:latest_post)
        |> Domain.read!()

      assert author.latest_post.id == post2.id
    end
  end

  describe "loading through attributes" do
    test "can load calculations through attributes" do
      Author
      |> Ash.Changeset.for_create(
        :create,
        %{name: "zerg", bio: %{first_name: "donald", last_name: "duck"}},
        authorize?: false
      )
      |> Domain.create!()

      assert [%{bio: %{full_name: "donald duck"}}] =
               Author
               |> Ash.Query.load(bio: :full_name)
               |> Domain.read!()
    end

    test "can load calculations through nil attributes" do
      Author
      |> Ash.Changeset.for_create(:create, %{name: "zerg"})
      |> Domain.create!()

      assert [%{bio: nil}] =
               Author
               |> Ash.Query.load(bio: :full_name)
               |> Domain.read!()
    end

    test "can load calculations through union" do
      Author
      |> Ash.Changeset.for_create(
        :create,
        %{name: "zerg", bio_union: %{type: "bio", first_name: "donald", last_name: "duck"}},
        authorize?: false
      )
      |> Domain.create!(authorize?: false)

      Author
      |> Ash.Changeset.for_create(
        :create,
        %{
          name: "zerg",
          bio_union: %{type: "other_kind_of_bio", first_name: "donald", last_name: "duck"}
        },
        authorize?: false
      )
      |> Domain.create!()

      assert [
               %{bio_union: %Ash.Union{value: %{full_name: "donald duck"}}},
               %{bio_union: %Ash.Union{value: %{full_name: "donald duck"}}}
             ] =
               Author
               |> Ash.Query.load(bio_union: [*: :full_name])
               |> Domain.read!()

      assert [
               %{bio_union: %Ash.Union{value: %{full_name: "donald duck"}}},
               %{bio_union: %Ash.Union{value: %{full_name: "donald duck"}}}
             ] =
               Author
               |> Ash.Query.load(bio_union: [bio: :full_name, other_kind_of_bio: :full_name])
               |> Domain.read!()
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
      |> Domain.create!()

      Author
      |> Ash.Changeset.for_create(
        :create,
        %{
          name: "zerg",
          bio_union: %{type: "other_kind_of_bio", first_name: "donald", last_name: "duck"}
        },
        authorize?: false
      )
      |> Domain.create!()

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
               |> Domain.read!(actor: %{name: "zerg"}, authorize?: true)
    end
  end
end
