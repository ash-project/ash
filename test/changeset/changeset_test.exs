defmodule Ash.Test.Changeset.ChangesetTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Changeset

  require Ash.Query

  defmodule Category do
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      read :default
      create :default, primary?: true

      create :create_with_confirmation do
        argument :confirm_name, :string
        change confirm(:name, :confirm_name)
      end
    end

    attributes do
      attribute :id, :uuid, primary_key?: true, default: &Ecto.UUID.generate/0
      attribute :name, :string
    end

    relationships do
      many_to_many :posts, Ash.Test.Changeset.ChangesetTest.Post,
        through: Ash.Test.Changeset.ChangesetTest.PostCategory,
        destination_field_on_join_table: :post_id,
        source_field_on_join_table: :category_id
    end
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
      read :default
      create :default
    end

    attributes do
      attribute :id, :uuid, primary_key?: true, default: &Ecto.UUID.generate/0
      attribute :name, :string
    end

    relationships do
      has_many :posts, Ash.Test.Changeset.ChangesetTest.Post, destination_field: :author_id

      has_many :composite_key_posts, Ash.Test.Changeset.ChangesetTest.CompositeKeyPost,
        destination_field: :author_id
    end
  end

  defmodule PostCategory do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      read :default
      create :default
    end

    relationships do
      belongs_to :post, Ash.Test.Changeset.ChangesetTest.Post, primary_key?: true
      belongs_to :category, Ash.Test.Changeset.ChangesetTest.Category, primary_key?: true
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      read :default
      create :default
      update :default
    end

    attributes do
      attribute :id, :uuid, primary_key?: true, default: &Ecto.UUID.generate/0
      attribute :title, :string
      attribute :contents, :string
    end

    relationships do
      belongs_to :author, Author

      many_to_many :categories, Ash.Test.Changeset.ChangesetTest.Category,
        through: Ash.Test.Changeset.ChangesetTest.PostCategory,
        destination_field_on_join_table: :category_id,
        source_field_on_join_table: :post_id
    end
  end

  defmodule CompositeKeyPost do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      read :default
      create :default
      update :default
    end

    attributes do
      attribute :serial, :integer, primary_key?: true
      attribute :id, :uuid, primary_key?: true, default: &Ecto.UUID.generate/0
      attribute :title, :string
      attribute :contents, :string
    end

    relationships do
      belongs_to :author, Author

      many_to_many :categories, Ash.Test.Changeset.ChangesetTest.Category,
        through: Ash.Test.Changeset.ChangesetTest.PostCategory,
        destination_field_on_join_table: :category_id,
        source_field_on_join_table: :post_id
    end
  end

  defmodule Api do
    @moduledoc false
    use Ash.Api

    resources do
      resource Category
      resource Author
      resource PostCategory
      resource Post
      resource CompositeKeyPost
    end
  end

  defmodule NonResource do
    @moduledoc false
    defstruct [:name]
  end

  describe "new" do
    test "it wraps a new resource in a `create` changeset" do
      assert %Changeset{
               action_type: :create,
               attributes: %{},
               data: %Category{},
               errors: [],
               valid?: true
             } =
               Category
               |> Changeset.new()
    end

    test "it wraps a resource's record in an `update` changeset" do
      record =
        Category
        |> Changeset.new(%{name: "foo"})
        |> Api.create!()

      assert %Changeset{
               action_type: :update,
               attributes: %{},
               data: %Category{name: "foo"},
               errors: [],
               valid?: true
             } =
               record
               |> Changeset.new()
    end

    test "it returns an error for a non-resource record" do
      assert %Changeset{
               action_type: :create,
               attributes: %{},
               data: %NonResource{},
               errors: [%Ash.Error.Invalid.NoSuchResource{}],
               valid?: false
             } =
               %NonResource{name: "foo"}
               |> Changeset.new()
    end
  end

  describe "with_hooks/2" do
    test "it applies a before_action function on a changeset" do
      capitalize_name = fn changeset = %Changeset{attributes: %{name: name}} ->
        %{
          changeset
          | attributes: Map.merge(changeset.attributes, %{name: String.capitalize(name)})
        }
      end

      changeset =
        Category
        |> Changeset.new(%{name: "foo"})

      changeset = %{changeset | before_action: [capitalize_name]}

      category = changeset |> Api.create!()

      assert %Category{name: "Foo"} = category
    end

    test "it applies a after_action function on a changeset" do
      capitalize_name = fn _changeset, result = %{name: _} ->
        {:ok, %{result | name: "modified"}}
      end

      changeset =
        Category
        |> Changeset.new(%{name: "foo"})

      changeset = %{changeset | after_action: [capitalize_name]}

      category = changeset |> Api.create!()
      assert %Category{name: "modified"} = category

      assert {:ok, %Category{name: "foo"}} = Api.get(Category, category.id)
    end
  end

  describe "get_attribute/2" do
    setup do
      category =
        Category
        |> Changeset.new(%{name: "foo"})
        |> Api.create!()

      {:ok, %{category: category}}
    end

    test "it get an attribute of the changeset", %{category: category} do
      changeset =
        category
        |> Changeset.new(%{name: "bar"})

      assert "bar" == Changeset.get_attribute(changeset, :name)
    end

    test "it falls back to the original value of the attribute", %{category: category} do
      changeset =
        category
        |> Changeset.new()

      assert "foo" == Changeset.get_attribute(changeset, :name)
    end

    test "it returns nil if the attribute not found" do
      changeset =
        Category
        |> Changeset.new()

      assert %{} == changeset.attributes

      assert is_nil(Changeset.get_attribute(changeset, :not_there))
    end
  end

  describe "fetch_change/2" do
    test "it get a changed attribute of the changeset" do
      category =
        Category
        |> Changeset.new(%{name: "foo"})
        |> Api.create!()

      changeset =
        category
        |> Changeset.new(%{name: "bar"})

      assert {:ok, "bar"} == Changeset.fetch_change(changeset, :name)
    end

    test "it returns :error if the attribute not found" do
      changeset =
        Category
        |> Changeset.new()

      assert %{} == changeset.attributes

      assert :error == Changeset.fetch_change(changeset, :none)
    end
  end

  describe "set_context/2" do
    test "it sets context to a given map" do
      changeset =
        Category
        |> Changeset.new()
        |> Changeset.set_context(%{key: "value"})

      assert "value" == changeset.context.key
    end
  end

  describe "put_context/2" do
    test "it puts a value in a context key" do
      changeset =
        Category
        |> Changeset.new()
        |> Changeset.put_context(:key, "value")

      assert "value" == changeset.context.key
    end
  end

  describe "append_to_relationship/3" do
    test "it appends entities to a resource's relationship" do
      post1 = Post |> Changeset.new(%{title: "title1"}) |> Api.create!()
      post2 = Post |> Changeset.new(%{title: "title2"}) |> Api.create!()

      changeset =
        Author
        |> Changeset.new()
        |> Changeset.append_to_relationship(:posts, [post1, post2])

      assert Enum.sort([%{id: post1.id}, %{id: post2.id}]) ==
               Enum.sort(changeset.relationships.posts.add)
    end

    test "it accepts value of single attribute primary_key as a second param" do
      post1 = Post |> Changeset.new(%{title: "title1"}) |> Api.create!()

      changeset =
        Author
        |> Changeset.new()
        |> Changeset.append_to_relationship(:posts, post1.id)

      assert %{add: [%{id: post1.id}]} == changeset.relationships.posts
    end

    test "it accepts a map %{id: value} representing primary key as a second param only if primary key is a single attribute" do
      post1 = Post |> Changeset.new(%{title: "foo"}) |> Api.create!()

      changeset =
        Author
        |> Changeset.new()
        |> Changeset.append_to_relationship(:posts, %{id: post1.id})

      assert %{add: [%{id: post1.id}]} == changeset.relationships.posts
    end

    test "it accepts many-to-many relationship" do
      post1 = Post |> Changeset.new(%{title: "foo"}) |> Api.create!()

      changeset =
        Category
        |> Changeset.new()
        |> Changeset.append_to_relationship(:posts, post1)

      assert %{add: [%{id: post1.id}]} == changeset.relationships.posts
    end

    test "it returns error if relationship does not exists" do
      post1 = Post |> Changeset.new(%{title: "foo"}) |> Api.create!()

      changeset =
        Author
        |> Changeset.new()
        |> Changeset.append_to_relationship(:na, post1)

      assert %{} == changeset.relationships
      assert [%Ash.Error.Changes.NoSuchRelationship{}] = changeset.errors
    end

    test "it reconciles after appending" do
      post1 = Post |> Changeset.new(%{title: "title1"}) |> Api.create!()
      post2 = Post |> Changeset.new(%{title: "title2"}) |> Api.create!()

      changeset =
        Author
        |> Changeset.new()
        |> Changeset.append_to_relationship(:posts, [post1])
        |> Changeset.append_to_relationship(:posts, [post2])
        |> Changeset.append_to_relationship(:posts, [post2])

      assert Enum.sort([%{id: post1.id}, %{id: post2.id}]) ==
               Enum.sort(changeset.relationships.posts.add)
    end
  end

  describe "replace_relationship/3" do
    test "it replaces entities to a resource's relationship" do
      post1 = Post |> Changeset.new(%{title: "title1"}) |> Api.create!()
      post2 = Post |> Changeset.new(%{title: "title2"}) |> Api.create!()

      author =
        Author
        |> Changeset.new()
        |> Changeset.replace_relationship(:posts, [post1])
        |> Api.create!()

      [author] =
        Author
        |> Ash.Query.load(posts: [:author])
        |> Ash.Query.filter(id == ^author.id)
        |> Api.read!()

      assert [author_post] = author.posts

      assert author_post.id == post1.id

      changeset =
        author
        |> Changeset.new()
        |> Changeset.replace_relationship(:posts, [post2])

      assert %{replace: [%{id: post2.id}]} == changeset.relationships.posts

      author =
        changeset
        |> Api.create!()

      [author] =
        Author
        |> Ash.Query.load(posts: [:author])
        |> Ash.Query.filter(id == ^author.id)
        |> Api.read!()

      assert [author_post] = author.posts

      assert author_post.id == post2.id
    end

    test "it puts relationship destination entities into changeset's context iff all entities are valid Ash resources" do
      post1 = Post |> Changeset.new(%{title: "title1"}) |> Api.create!()
      post2 = Post |> Changeset.new(%{title: "title2"}) |> Api.create!()

      changeset =
        Author
        |> Changeset.new()
        |> Changeset.replace_relationship(:posts, [post1, post2.id])

      refute Map.get(changeset.context, :destination_entities)

      changeset =
        Author
        |> Changeset.new()
        |> Changeset.replace_relationship(:posts, [post1, post2])

      assert get_in(changeset.context, [:destination_entities, :posts, Post]) == [post1, post2]
    end

    test "it accepts value of single attribute primary_key as a second param" do
      post1 = Post |> Changeset.new(%{title: "title1"}) |> Api.create!()

      changeset =
        Author
        |> Changeset.new()
        |> Changeset.replace_relationship(:posts, post1.id)

      assert %{replace: [%{id: post1.id}]} == changeset.relationships.posts
    end

    test "it accepts a map %{id: value} representing primary key as a second param" do
      post1 =
        Post
        |> Changeset.new(%{title: "foo"})
        |> Api.create!()

      changeset =
        Author
        |> Changeset.new()
        |> Changeset.replace_relationship(:posts, %{id: post1.id})

      assert %{replace: [%{id: post1.id}]} == changeset.relationships.posts
    end

    test "it accepts a map %{att1: value1, att2: value2} representing primary key as a second param" do
      post1 =
        CompositeKeyPost
        |> Changeset.new(%{serial: 1})
        |> Api.create!()

      changeset =
        Author
        |> Changeset.new()
        |> Changeset.replace_relationship(
          :composite_key_posts,
          %{id: post1.id, serial: post1.serial}
        )

      assert %{replace: [%{id: post1.id, serial: post1.serial}]} ==
               changeset.relationships.composite_key_posts

      assert [] == changeset.errors

      author =
        changeset
        |> Api.create!()

      [fetched_post] =
        CompositeKeyPost
        |> Ash.Query.load(author: :composite_key_posts)
        |> Ash.Query.filter(id == ^post1.id and serial == ^post1.serial)
        |> Api.read!()

      assert author == fetched_post.author
    end

    test "it accepts a list of maps representing primary_keys as a second param" do
      post1 =
        CompositeKeyPost
        |> Changeset.new(%{serial: 1})
        |> Api.create!()

      post2 =
        CompositeKeyPost
        |> Changeset.new(%{serial: 2})
        |> Api.create!()

      changeset =
        Author
        |> Changeset.new()
        |> Changeset.replace_relationship(
          :composite_key_posts,
          [
            %{id: post1.id, serial: post1.serial},
            %{id: post2.id, serial: post2.serial}
          ]
        )

      assert Enum.sort([
               %{id: post1.id, serial: post1.serial},
               %{id: post2.id, serial: post2.serial}
             ]) ==
               Enum.sort(changeset.relationships.composite_key_posts.replace)

      assert [] == changeset.errors

      author =
        changeset
        |> Api.create!()

      [fetched_post] =
        CompositeKeyPost
        |> Ash.Query.load(author: :composite_key_posts)
        |> Ash.Query.filter(id == ^post1.id and serial == ^post1.serial)
        |> Api.read!()

      assert author == fetched_post.author
    end

    test "it accepts mix of entities and maps representing primary_keys as a second param" do
      post1 =
        CompositeKeyPost
        |> Changeset.new(%{serial: 1})
        |> Api.create!()

      post2 =
        CompositeKeyPost
        |> Changeset.new(%{serial: 2})
        |> Api.create!()

      changeset =
        Author
        |> Changeset.new()
        |> Changeset.replace_relationship(
          :composite_key_posts,
          [
            %{id: post1.id, serial: post1.serial},
            post2
          ]
        )

      assert Enum.sort([
               %{id: post1.id, serial: post1.serial},
               %{id: post2.id, serial: post2.serial}
             ]) ==
               Enum.sort(changeset.relationships.composite_key_posts.replace)

      assert [] == changeset.errors

      author =
        changeset
        |> Api.create!()

      [fetched_author] =
        Author
        |> Ash.Query.load(:composite_key_posts)
        |> Ash.Query.filter(id == ^author.id)
        |> Api.read!()

      assert Enum.sort(Enum.map(fetched_author.composite_key_posts, & &1.id)) ==
               Enum.sort([post1.id, post2.id])
    end

    test "it returns error if one of relationship entities is invalid" do
      post1 =
        CompositeKeyPost
        |> Changeset.new(%{serial: 1})
        |> Api.create!()

      post2 =
        CompositeKeyPost
        |> Changeset.new(%{serial: 2})
        |> Api.create!()

      invalid_post =
        Post
        |> Changeset.new(%{title: "a title"})
        |> Api.create!()

      changeset =
        Author
        |> Changeset.new()
        |> Changeset.replace_relationship(
          :composite_key_posts,
          [
            %{id: post1.id, serial: post1.serial},
            post2,
            invalid_post
          ]
        )

      assert Enum.empty?(changeset.relationships)

      assert [%Ash.Error.Changes.InvalidRelationship{} = relation_error] = changeset.errors
      assert relation_error.message =~ "Invalid identifier"
    end

    test "it accepts many-to-many relationship" do
      post1 =
        Post
        |> Changeset.new(%{title: "foo"})
        |> Api.create!()

      changeset =
        Category
        |> Changeset.new()
        |> Changeset.replace_relationship(:posts, post1)

      assert %{replace: [%{id: post1.id}]} == changeset.relationships.posts
    end

    test "it returns error if relationship does not exists" do
      post1 = Post |> Changeset.new(%{title: "foo"}) |> Api.create!()

      changeset =
        Author
        |> Changeset.new()
        |> Changeset.replace_relationship(:na, post1)

      assert %{} == changeset.relationships
      assert [%Ash.Error.Changes.NoSuchRelationship{}] = changeset.errors
    end

    test "it reconciles after replacement" do
      post1 = Post |> Changeset.new(%{title: "title1"}) |> Api.create!()
      post2 = Post |> Changeset.new(%{title: "title2"}) |> Api.create!()

      changeset =
        Author
        |> Changeset.new()
        |> Changeset.replace_relationship(:posts, [post1])
        |> Changeset.replace_relationship(:posts, [post2])

      assert Enum.sort([%{id: post2.id}]) ==
               Enum.sort(changeset.relationships.posts.replace)

      changeset =
        changeset
        |> Changeset.replace_relationship(:posts, [])

      assert [] == changeset.relationships.posts.replace
    end
  end

  describe "changing_attribute?/2" do
    test "it returns true if the attribute is being changed by the current changeset" do
      changeset = Post |> Changeset.new(%{title: "title1"})
      assert Changeset.changing_attribute?(changeset, :title)
    end

    test "it returns false if the attribute is NOT being changed by the current changeset" do
      changeset = Post |> Changeset.new(%{title: "title1"})
      refute Changeset.changing_attribute?(changeset, :contents)
    end
  end

  describe "changing_relationship?/2" do
    test "it returns true if the attribute is being changed by the current changeset" do
      post = Post |> Changeset.new(%{title: "title2"}) |> Api.create!()

      changeset =
        Author
        |> Changeset.new()
        |> Changeset.replace_relationship(:posts, [post])

      assert Changeset.changing_relationship?(changeset, :posts)
    end

    test "it returns false if the attribute is NOT being changed by the current changeset" do
      changeset = Post |> Changeset.new(%{title: "title1"})
      refute Changeset.changing_relationship?(changeset, :posts)
    end
  end

  describe "change_new_attribute/3" do
    test "it changes attribute if it's not currently being changed" do
      changeset =
        Post
        |> Changeset.new(%{title: "title1"})
        |> Changeset.change_new_attribute(:contents, "some content")

      assert %Changeset{attributes: %{title: "title1", contents: "some content"}} = changeset
    end

    test "it keeps the current value of attribute if it's currently being changed" do
      changeset =
        Post
        |> Changeset.new(%{title: "title1"})
        |> Changeset.change_new_attribute(:title, "another title")

      assert %Changeset{
               attributes: %{
                 title: "title1"
               }
             } = changeset
    end
  end

  describe "arguments" do
    test "arguments can be used in valid changes" do
      Category
      |> Changeset.new(%{"name" => "foo"})
      |> Changeset.set_argument(:confirm_name, "foo")
      |> Api.create!(action: :create_with_confirmation)
    end

    test "arguments can be used in invalid changes" do
      assert_raise Ash.Error.Invalid, ~r/Value did not match confirmation/, fn ->
        Category
        |> Changeset.new(%{"name" => "foo"})
        |> Changeset.set_argument(:confirm_name, "bar")
        |> Api.create!(action: :create_with_confirmation)
      end
    end
  end
end
