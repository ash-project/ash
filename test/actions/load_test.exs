defmodule Ash.Test.Actions.LoadTest do
  @moduledoc false
  use ExUnit.Case, async: false

  import Ash.Changeset
  require Ash.Query

  defmodule Campaign do
    @moduledoc false
    use Ash.Resource,
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

  defmodule Author do
    @moduledoc false
    use Ash.Resource,
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

    def load(posts, _, %{query: destination_query, api: api}) do
      categories = Enum.map(posts, & &1.category)

      other_posts =
        destination_query
        |> Ash.Query.filter(category in ^categories)
        |> api.read!()
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
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

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
      define_for(Ash.Test.Actions.LoadTest.Api)

      define :get_by_id do
        action(:read)
        get_by([:id])
      end
    end

    relationships do
      belongs_to(:author, Author)

      has_many :posts_in_same_category, __MODULE__ do
        manual(PostsInSameCategory)
      end

      has_many :ratings, Ash.Test.Actions.LoadTest.Rating do
        api(Ash.Test.Actions.LoadTest.Api2)
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
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

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
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

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
        api(Ash.Test.Actions.LoadTest.Api)
      end
    end
  end

  defmodule Registry do
    @moduledoc false
    use Ash.Registry

    entries do
      entry(Author)
      entry(Post)
      entry(Category)
      entry(PostCategory)
      entry(Campaign)
    end
  end

  defmodule Registry2 do
    @moduledoc false
    use Ash.Registry

    entries do
      entry(Rating)
    end
  end

  defmodule Api do
    @moduledoc false
    use Ash.Api

    resources do
      registry(Registry)
    end
  end

  defmodule Api2 do
    @moduledoc false
    use Ash.Api

    resources do
      registry(Registry2)
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
        |> new(%{title: "post1", category: "foo"})
        |> Api.create!()

      Post
      |> new(%{title: "post2", category: "bar"})
      |> Api.create!()

      post3 =
        Post
        |> new(%{title: "post2", category: "foo"})
        |> Api.create!()

      post3_id = post3.id

      assert [%{id: ^post3_id}] =
               post1
               |> Api.load!(:posts_in_same_category)
               |> Map.get(:posts_in_same_category)
    end

    test "parent expressions can be used for complex constraints" do
      post1 =
        Post
        |> new(%{title: "post1", category: "foo"})
        |> Api.create!()

      post1_same =
        Post
        |> new(%{title: "post1", category: "bar"})
        |> Api.create!()

      Post
      |> new(%{title: "post2", category: "baz"})
      |> Api.create!()

      post1_same_id = post1_same.id

      assert [%{id: ^post1_same_id}] =
               post1
               |> Api.load!(:posts_with_same_title)
               |> Map.get(:posts_with_same_title)
    end

    test "it allows loading through manual relationships" do
      post1 =
        Post
        |> new(%{title: "post1", category: "foo"})
        |> Api.create!()

      Post
      |> new(%{title: "post2", category: "bar"})
      |> Api.create!()

      post3 =
        Post
        |> new(%{title: "post2", category: "foo"})
        |> Api.create!()

      post3_id = post3.id

      assert [%{id: ^post3_id}] =
               post1
               |> Api.load!(posts_in_same_category: :ratings)
               |> Map.get(:posts_in_same_category)
    end

    test "it uses `Comp.equal?/2` to support things like ci_string foreign keys" do
      author =
        Author
        |> new(%{name: "zerg", campaign_name: "FrEd"})
        |> Api.create!()

      Campaign
      |> new(%{name: "fReD"})
      |> Api.create!()

      assert %{
               campaign: %{name: %Ash.CiString{string: "fReD"}},
               campaign_name: %Ash.CiString{string: "FrEd"}
             } = Api.load!(author, :campaign)
    end

    test "it allows loading related data" do
      author =
        Author
        |> new(%{name: "zerg"})
        |> Api.create!()

      post1 =
        Post
        |> new(%{title: "post1"})
        |> manage_relationship(:author, author, type: :append_and_remove)
        |> Api.create!()

      post2 =
        Post
        |> new(%{title: "post2"})
        |> manage_relationship(:author, author, type: :append_and_remove)
        |> Api.create!()

      [author] =
        Author
        |> Ash.Query.load(posts: [:author])
        |> Ash.Query.filter(posts.id == ^post1.id)
        |> Api.read!(authorize?: true)

      assert Enum.sort(Enum.map(author.posts, &Map.get(&1, :id))) ==
               Enum.sort([post1.id, post2.id])

      for post <- author.posts do
        assert post.author.id == author.id
      end
    end

    test "unloading related data sets it back to `%Ash.NotLoaded{}`" do
      author =
        Author
        |> new(%{name: "zerg"})
        |> Api.create!()

      post1 =
        Post
        |> new(%{title: "post1"})
        |> manage_relationship(:author, author, type: :append_and_remove)
        |> Api.create!()

      Post
      |> new(%{title: "post2"})
      |> manage_relationship(:author, author, type: :append_and_remove)
      |> Api.create!()

      [author] =
        Author
        |> Ash.Query.load(posts: [:author])
        |> Ash.Query.filter(posts.id == ^post1.id)
        |> Api.read!(authorize?: true)

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
        |> new(%{name: "zerg"})
        |> Api.create!()

      Post
      |> new(%{title: "post1"})
      |> manage_relationship(:author, author, type: :append_and_remove)
      |> Api.create!()

      Post
      |> new(%{title: "post2"})
      |> manage_relationship(:author, author, type: :append_and_remove)
      |> Api.create!()

      [author] =
        Author
        |> Ash.Query.load(:posts)
        |> Api.read!(authorize?: true)
        |> Api.load!(:latest_post)

      refute match?(%Ash.NotLoaded{}, author.posts)
      refute match?(%Ash.NotLoaded{}, author.latest_post)
    end

    test "loading something already loaded still loads it unless lazy?: true" do
      author =
        Author
        |> new(%{name: "zerg"})
        |> Api.create!()

      post1 =
        Post
        |> new(%{title: "post1"})
        |> manage_relationship(:author, author, type: :append_and_remove)
        |> Api.create!()

      post2 =
        Post
        |> new(%{title: "post2"})
        |> manage_relationship(:author, author, type: :append_and_remove)
        |> Api.create!()

      [author] =
        Author
        |> Ash.Query.load(posts: [:author])
        |> Ash.Query.filter(posts.id == ^post1.id)
        |> Api.read!(authorize?: true)

      assert Enum.sort(Enum.map(author.posts, &Map.get(&1, :id))) ==
               Enum.sort([post1.id, post2.id])

      for post <- author.posts do
        assert post.author.id == author.id
      end

      post3 =
        Post
        |> new(%{title: "post3"})
        |> manage_relationship(:author, author, type: :append_and_remove)
        |> Api.create!()

      author =
        author
        |> Api.load!([posts: [:author]], authorize?: true)

      assert Enum.sort(Enum.map(author.posts, &Map.get(&1, :id))) ==
               Enum.sort([post1.id, post2.id, post3.id])

      for post <- author.posts do
        assert post.author.id == author.id
      end

      Post
      |> new(%{title: "post4"})
      |> manage_relationship(:author, author, type: :append_and_remove)
      |> Api.create!()

      author =
        author
        |> Api.load!([posts: [:author]], authorize?: true, lazy?: true)

      assert Enum.sort(Enum.map(author.posts, &Map.get(&1, :id))) ==
               Enum.sort([post1.id, post2.id, post3.id])

      for post <- author.posts do
        assert post.author.id == author.id
      end
    end

    test "nested lazy loads work" do
      author =
        Author
        |> new(%{name: "zerg"})
        |> Api.create!()

      post1 =
        Post
        |> new(%{title: "post1"})
        |> manage_relationship(:author, author, type: :append_and_remove)
        |> Api.create!()

      Post
      |> new(%{title: "post2"})
      |> manage_relationship(:author, author, type: :append_and_remove)
      |> Api.create!()

      author =
        Author
        |> Ash.Query.load(:posts)
        |> Ash.Query.filter(posts.id == ^post1.id)
        |> Api.read_one!(authorize?: true)
        |> Api.load!([posts: :author], lazy?: true)

      author_id = author.id

      assert %{posts: [%{author: %{id: ^author_id}}, %{author: %{id: ^author_id}}]} = author

      post =
        Post
        |> Ash.Query.load(:author)
        |> Ash.Query.filter(id == ^post1.id)
        |> Api.read_one!()
        |> Api.load!([author: :posts], lazy?: true)

      assert %{author: %{posts: [_, _]}} = post
    end

    test "it allows loading across APIs" do
      author =
        Author
        |> new(%{name: "zerg"})
        |> Api.create!()

      post =
        Post
        |> new(%{title: "post1"})
        |> manage_relationship(:author, author, type: :append_and_remove)
        |> Api.create!()

      rating =
        Rating
        |> new(%{rating: 10})
        |> manage_relationship(:post, post, type: :append_and_remove)
        |> Api2.create!()

      assert [loaded_rating] =
               author
               |> Api.load!(posts: :ratings)
               |> Map.get(:posts)
               |> Enum.at(0)
               |> Map.get(:ratings)

      assert loaded_rating.id == rating.id
    end

    test "it allows loading many to many relationships" do
      category1 =
        Category
        |> new(%{name: "lame"})
        |> Api.create!()

      category2 =
        Category
        |> new(%{name: "cool"})
        |> Api.create!()

      post =
        Post
        |> new(%{title: "post1"})
        |> manage_relationship(:categories, [category1, category2], type: :append_and_remove)
        |> Api.create!()

      [post] =
        Post
        |> Ash.Query.load(:categories)
        |> Ash.Query.filter(id == ^post.id)
        |> Api.read!(authorize?: true)

      assert [%{id: id1}, %{id: id2}] = post.categories

      assert Enum.sort([category1.id, category2.id]) == Enum.sort([id1, id2])
    end

    test "it allows loading nested many to many relationships lazily" do
      category1 =
        Category
        |> new(%{name: "lame"})
        |> Api.create!()

      category2 =
        Category
        |> new(%{name: "cool"})
        |> Api.create!()

      post =
        Post
        |> new(%{title: "post1"})
        |> manage_relationship(:categories, [category1, category2], type: :append_and_remove)
        |> Api.create!()

      assert [_] =
               Post
               |> Ash.Query.load(:categories)
               |> Ash.Query.filter(id == ^post.id)
               |> Api.read!(authorize?: true)

      assert %{posts: [%{categories: [_, _]}]} =
               Category
               |> Ash.Query.filter(id == ^category1.id)
               |> Ash.Query.load(:posts)
               |> Api.read_one!()
               |> Api.load!([posts: :categories], lazy?: true)
    end

    test "it allows loading many to many relationships after the fact" do
      category1 =
        Category
        |> new(%{name: "lame"})
        |> Api.create!()

      category2 =
        Category
        |> new(%{name: "cool"})
        |> Api.create!()

      post =
        Post
        |> new(%{title: "post1"})
        |> manage_relationship(:categories, [category1, category2], type: :append_and_remove)
        |> Api.create!()

      post = Post.get_by_id!(post.id, load: [:categories])

      assert [%{id: id1}, %{id: id2}] = post.categories

      assert Enum.sort([category1.id, category2.id]) == Enum.sort([id1, id2])
    end

    test "it produces a nice error message on loading invalid loads" do
      assert_raise Ash.Error.Invalid, ~r/:non_existent_thing is not a valid load/, fn ->
        Post
        |> Ash.Query.load(categories: [posts: :non_existent_thing])
        |> Api.read!(authorize?: true)
      end
    end

    test "it allows loading nested many to many relationships" do
      category1 =
        Category
        |> new(%{name: "lame"})
        |> Api.create!()

      category2 =
        Category
        |> new(%{name: "cool"})
        |> Api.create!()

      post =
        Post
        |> new(%{title: "post1"})
        |> manage_relationship(:categories, [category1, category2], type: :append_and_remove)
        |> Api.create!()

      [post] =
        Post
        |> Ash.Query.load(categories: :posts)
        |> Ash.Query.filter(id == ^post.id)
        |> Api.read!(authorize?: true)

      post_id = post.id

      assert [%{posts: [%{id: ^post_id}]}, %{posts: [%{id: ^post_id}]}] = post.categories
    end

    test "it loads sorted relationships in the proper order" do
      author =
        Author
        |> new(%{name: "zerg"})
        |> Api.create!()

      _post1 =
        Post
        |> new(%{title: "post1"})
        |> manage_relationship(:author, author, type: :append_and_remove)
        |> Api.create!()

      post2 =
        Post
        |> new(%{title: "post2"})
        |> manage_relationship(:author, author, type: :append_and_remove)
        |> Api.create!()

      [author] =
        Author
        |> Ash.Query.load(:latest_post)
        |> Api.read!()

      assert author.latest_post.id == post2.id
    end
  end

  describe "loading through attributes" do
    test "can load calculations through attributes" do
      Author
      |> new(%{name: "zerg", bio: %{first_name: "donald", last_name: "duck"}})
      |> Api.create!()

      assert [%{bio: %{full_name: "donald duck"}}] =
               Author
               |> Ash.Query.load(bio: :full_name)
               |> Api.read!()
    end

    test "can load calculations through nil attributes" do
      Author
      |> new(%{name: "zerg"})
      |> Api.create!()

      assert [%{bio: nil}] =
               Author
               |> Ash.Query.load(bio: :full_name)
               |> Api.read!()
    end

    test "can load calculations through union" do
      Author
      |> new(%{name: "zerg", bio_union: %{type: "bio", first_name: "donald", last_name: "duck"}})
      |> Api.create!()

      Author
      |> new(%{
        name: "zerg",
        bio_union: %{type: "other_kind_of_bio", first_name: "donald", last_name: "duck"}
      })
      |> Api.create!()

      assert [
               %{bio_union: %Ash.Union{value: %{full_name: "donald duck"}}},
               %{bio_union: %Ash.Union{value: %{full_name: "donald duck"}}}
             ] =
               Author
               |> Ash.Query.load(bio_union: [*: :full_name])
               |> Api.read!()

      assert [
               %{bio_union: %Ash.Union{value: %{full_name: "donald duck"}}},
               %{bio_union: %Ash.Union{value: %{full_name: "donald duck"}}}
             ] =
               Author
               |> Ash.Query.load(bio_union: [bio: :full_name, other_kind_of_bio: :full_name])
               |> Api.read!()
    end

    test "can load calculations through union produced by a calculation" do
      Author
      |> new(%{name: "zerg", bio_union: %{type: "bio", first_name: "donald", last_name: "duck"}})
      |> Api.create!()

      Author
      |> new(%{
        name: "zerg",
        bio_union: %{type: "other_kind_of_bio", first_name: "donald", last_name: "duck"}
      })
      |> Api.create!()

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
               |> Api.read!(actor: %{name: "zerg"}, authorize?: true)

      # assert [
      #          %{
      #            bio_union_calc: %Ash.Union{
      #              value: %{full_name: "donald duck", forbidden_name: %Ash.ForbiddenField{}}
      #            }
      #          },
      #          %{
      #            bio_union_calc: %Ash.Union{
      #              value: %{full_name: "donald duck", forbidden_name: %Ash.ForbiddenField{}}
      #            }
      #          }
      #        ] =
      #          Author
      #          |> Ash.Query.load(
      #            bio_union_calc:
      #              {%{}, [bio: :full_name, other_kind_of_bio: [:full_name, :forbidden_name]]}
      #          )
      #          |> Api.read!(actor: %{name: "zerg"})
    end
  end
end
