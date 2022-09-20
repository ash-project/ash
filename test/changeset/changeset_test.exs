defmodule Ash.Test.Changeset.ChangesetTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Changeset
  alias Ash.Test.AnyApi, as: Api

  require Ash.Query

  defmodule Category do
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    identities do
      identity :unique_name, [:name], pre_check_with: Api
    end

    actions do
      defaults [:read, :create, :update, :destroy]

      create :create_with_confirmation do
        argument :confirm_name, :string do
          allow_nil? false
        end

        argument :false_optional_argument, :boolean do
          allow_nil? false
          default false
        end

        argument :true_optional_argument, :boolean do
          allow_nil? false
          default true
        end

        validate confirm(:name, :confirm_name)
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string
    end

    relationships do
      many_to_many :posts, Ash.Test.Changeset.ChangesetTest.Post,
        through: Ash.Test.Changeset.ChangesetTest.PostCategory,
        destination_attribute_on_join_resource: :post_id,
        source_attribute_on_join_resource: :category_id
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
      defaults [:read, :create, :update, :destroy]
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string
    end

    relationships do
      has_many :posts, Ash.Test.Changeset.ChangesetTest.Post, destination_attribute: :author_id

      has_many :unique_posts, Ash.Test.Changeset.ChangesetTest.UniqueNamePerAuthor,
        destination_attribute: :author_id

      has_many :composite_key_posts, Ash.Test.Changeset.ChangesetTest.CompositeKeyPost,
        destination_attribute: :author_id
    end
  end

  defmodule PostCategory do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      defaults [:read, :create, :update, :destroy]
    end

    relationships do
      belongs_to :post, Ash.Test.Changeset.ChangesetTest.Post,
        primary_key?: true,
        allow_nil?: false

      belongs_to :category, Ash.Test.Changeset.ChangesetTest.Category,
        primary_key?: true,
        allow_nil?: false
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      defaults [:read, :create, :update, :destroy]
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string
      attribute :contents, :string
    end

    relationships do
      belongs_to :author, Author

      many_to_many :categories, Ash.Test.Changeset.ChangesetTest.Category,
        through: Ash.Test.Changeset.ChangesetTest.PostCategory,
        destination_attribute_on_join_resource: :category_id,
        source_attribute_on_join_resource: :post_id
    end
  end

  defmodule CompositeKeyPost do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      defaults [:read, :create, :update, :destroy]
    end

    attributes do
      attribute :serial, :integer, primary_key?: true, allow_nil?: false
      uuid_primary_key :id
      attribute :title, :string
      attribute :contents, :string
    end

    relationships do
      belongs_to :author, Author

      many_to_many :categories, Ash.Test.Changeset.ChangesetTest.Category,
        through: Ash.Test.Changeset.ChangesetTest.PostCategory,
        destination_attribute_on_join_resource: :category_id,
        source_attribute_on_join_resource: :post_id
    end
  end

  defmodule UniqueNamePerAuthor do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      defaults [:read, :create, :update, :destroy]
    end

    identities do
      identity :unique_name_per_author, [:title, :author_id], pre_check_with: Api
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string
      attribute :contents, :string
    end

    relationships do
      belongs_to :author, Author

      many_to_many :categories, Ash.Test.Changeset.ChangesetTest.Category,
        through: Ash.Test.Changeset.ChangesetTest.PostCategory,
        destination_attribute_on_join_resource: :category_id,
        source_attribute_on_join_resource: :post_id
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

    test "if an attribute does not exist in `new/2`, a corresponding error is added`" do
      error =
        Author
        |> Changeset.new(%{"flarb" => 10})
        |> Map.get(:errors)
        |> Enum.at(0)

      assert %Ash.Error.Changes.NoSuchAttribute{name: "flarb"} = error
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

    test "it raises an error for a non-resource record" do
      assert_raise ArgumentError, ~r/No such entity/, fn ->
        %NonResource{name: "foo"}
        |> Changeset.new()
      end
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

  describe "manage_relationship/3" do
    test "it works for belongs_to relationships" do
      author = %{name: "title"}

      post =
        Post
        |> Changeset.new()
        |> Changeset.manage_relationship(:author, author, on_no_match: :create)
        |> Api.create!()

      assert [%{name: "title"}] = Api.read!(Author)

      assert %{name: "title"} = Api.load!(post, :author).author
    end

    test "it removes belongs_to entities on replace" do
      author = %{name: "title"}

      post =
        Post
        |> Changeset.new()
        |> Changeset.manage_relationship(:author, author, on_no_match: :create)
        |> Api.create!()

      new_author = %{name: "title2"}

      post =
        post
        |> Changeset.new()
        |> Changeset.manage_relationship(:author, new_author,
          on_no_match: :create,
          on_missing: :destroy
        )
        |> Api.update!()

      assert [%{name: "title2"}] = Api.read!(Author)

      assert %{name: "title2"} = Api.load!(post, :author).author
    end

    test "upsert with many_to_many relationships creates and relates records, and returns the created/related records" do
      Category
      |> Changeset.new(name: "foo")
      |> Api.create!()

      post =
        Post
        |> Changeset.new()
        |> Changeset.manage_relationship(:categories, [%{name: "foo"}, %{name: "bar"}],
          on_lookup: :relate,
          on_no_match: :create
        )
        |> Api.create!()

      assert [%{name: "bar"}, %{name: "foo"}] = Enum.sort_by(post.categories, & &1.name)
    end

    test "value_is_key option determines what single values are keyed as" do
      changeset =
        Post
        |> Changeset.new()
        |> Changeset.manage_relationship(:categories, ["foo", "bar"],
          on_lookup: :relate,
          on_no_match: :create
        )

      inputs = changeset.relationships.categories |> Enum.at(0) |> elem(0)

      assert inputs == [%{id: "foo"}, %{id: "bar"}]

      changeset =
        Post
        |> Changeset.new()
        |> Changeset.manage_relationship(:categories, ["foo", "bar"],
          value_is_key: :name,
          on_lookup: :relate,
          on_no_match: :create
        )

      inputs = changeset.relationships.categories |> Enum.at(0) |> elem(0)

      assert inputs == [%{name: "foo"}, %{name: "bar"}]
    end

    test "upsert with many_to_many relationships can eager validate" do
      Category
      |> Changeset.new(name: "foo")
      |> Api.create!()

      assert %{valid?: false, errors: [%Ash.Error.Query.NotFound{}]} =
               Post
               |> Changeset.new()
               |> Changeset.manage_relationship(:categories, [%{name: "foo"}, %{name: "bar"}],
                 on_lookup: :relate,
                 eager_validate_with: Api
               )

      assert %{valid?: true} =
               Post
               |> Changeset.new()
               |> Changeset.manage_relationship(:categories, [%{name: "foo"}],
                 on_lookup: :relate,
                 eager_validate_with: Api
               )
    end

    test "it creates related entities" do
      post1 = %{title: "title"}
      post2 = %{title: "title"}

      author =
        Author
        |> Changeset.new()
        |> Changeset.manage_relationship(:posts, [post1, post2], on_no_match: :create)
        |> Api.create!()

      assert [%{title: "title"}, %{title: "title"}] = Api.read!(Post)

      assert %{posts: [%{title: "title"}, %{title: "title"}]} = Api.load!(author, :posts)
    end

    test "it ignores creates if configured" do
      post1 = %{title: "title"}
      post2 = %{title: "title"}

      author =
        Author
        |> Changeset.new()
        |> Changeset.manage_relationship(:posts, [post1, post2], on_no_match: :ignore)
        |> Api.create!()

      assert [] = Api.read!(Post)

      assert %{posts: []} = Api.load!(author, :posts)
    end

    test "it errors on creates if configured" do
      post1 = %{title: "title"}
      post2 = %{title: "title"}

      assert_raise Ash.Error.Invalid,
                   ~r/Invalid value provided for posts: Changes would create a new related record/,
                   fn ->
                     Author
                     |> Changeset.new()
                     |> Changeset.manage_relationship(:posts, [post1, post2], on_no_match: :error)
                     |> Api.create!()
                   end
    end

    test "it destroys related records if not present" do
      post1 = %{title: "title"}
      post2 = %{title: "title"}

      author =
        Author
        |> Changeset.new()
        |> Changeset.manage_relationship(:posts, [post1, post2], on_no_match: :create)
        |> Api.create!()

      assert [%{title: "title"}, %{title: "title"}] = Api.read!(Post)

      author
      |> Changeset.new()
      |> Changeset.manage_relationship(:posts, [], on_missing: :destroy)
      |> Api.update!()

      assert [] = Api.read!(Post)
    end

    test "it unrelates records if specified" do
      post1 = %{title: "title"}
      post2 = %{title: "title1"}

      author =
        Author
        |> Changeset.new()
        |> Changeset.manage_relationship(:unique_posts, [post1, post2], on_no_match: :create)
        |> Api.create!()

      assert [%{title: "title"}, %{title: "title1"}] =
               Enum.sort_by(Api.read!(UniqueNamePerAuthor), & &1.title)

      author =
        author
        |> Changeset.new()
        |> Changeset.manage_relationship(:unique_posts, [%{title: "title"}], on_missing: :unrelate)
        |> Api.update!()

      assert [%{title: "title"}, %{title: "title1"}] =
               Enum.sort_by(Api.read!(UniqueNamePerAuthor), & &1.title)

      assert %{unique_posts: [%{title: "title"}]} = Api.load!(author, :unique_posts)
    end

    test "it properly assumes the destination field of the relationship matches records when not provided" do
      post1 = %{title: "title"}
      post2 = %{title: "title"}

      author =
        Author
        |> Changeset.new()
        |> Changeset.manage_relationship(:posts, [post1, post2], on_no_match: :create)
        |> Api.create!()

      assert [%{title: "title"}, %{title: "title"}] = Api.read!(Post)

      author =
        author
        |> Changeset.new()
        |> Changeset.manage_relationship(:posts, [], on_missing: :unrelate)
        |> Api.update!()

      assert [%{title: "title"}, %{title: "title"}] = Api.read!(Post)

      assert %{posts: []} = Api.load!(author, :posts)
    end

    test "it updates records" do
      post1 = %{title: "title"}
      post2 = %{title: "title"}

      author =
        Author
        |> Changeset.new()
        |> Changeset.manage_relationship(:posts, [post1, post2], on_no_match: :create)
        |> Api.create!()

      assert posts = [%{title: "title"}, %{title: "title"}] = Api.read!(Post)

      post_ids = Enum.map(posts, &Map.get(&1, :id))
      input = Enum.map(post_ids, &%{"id" => &1, title: "new_title"})

      author
      |> Changeset.new()
      |> Changeset.manage_relationship(:posts, input, on_match: :update)
      |> Api.update!()

      assert [%{title: "new_title"}, %{title: "new_title"}] = Api.read!(Post)
    end
  end

  describe "manage_relationship/3 type: :append_and_remove" do
    test "it replaces entities to a resource's relationship" do
      post1 = Post |> Changeset.new(%{title: "title1"}) |> Api.create!()
      post2 = Post |> Changeset.new(%{title: "title2"}) |> Api.create!()

      author =
        Author
        |> Changeset.new()
        |> Changeset.manage_relationship(:posts, [post1], type: :append_and_remove)
        |> Api.create!()

      [author] =
        Author
        |> Ash.Query.load(posts: [:author])
        |> Ash.Query.filter(id == ^author.id)
        |> Api.read!()

      assert [author_post] = author.posts

      assert author_post.id == post1.id

      author =
        author
        |> Changeset.new()
        |> Changeset.manage_relationship(:posts, [post2], type: :append_and_remove)
        |> Api.update!()

      [author] =
        Author
        |> Ash.Query.load(posts: [:author])
        |> Ash.Query.filter(id == ^author.id)
        |> Api.read!()

      assert [author_post] = author.posts

      assert author_post.id == post2.id
    end

    test "it accepts a map %{att1: value1, att2: value2} representing primary key as a second param" do
      post1 =
        CompositeKeyPost
        |> Changeset.new(%{serial: 1})
        |> Api.create!()

      author =
        Author
        |> Changeset.new()
        |> Changeset.manage_relationship(
          :composite_key_posts,
          [%{id: post1.id, serial: post1.serial}],
          type: :append_and_remove
        )
        |> Api.create!()

      [fetched_post] =
        CompositeKeyPost
        |> Ash.Query.load(author: :composite_key_posts)
        |> Ash.Query.filter(id == ^post1.id and serial == ^post1.serial)
        |> Api.read!()

      assert Api.reload!(author) == Api.reload!(fetched_post.author)
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

      author =
        Author
        |> Changeset.new()
        |> Changeset.manage_relationship(
          :composite_key_posts,
          [
            %{id: post1.id, serial: post1.serial},
            %{id: post2.id, serial: post2.serial}
          ],
          type: :append_and_remove
        )
        |> Api.create!()

      [fetched_post] =
        CompositeKeyPost
        |> Ash.Query.load(author: :composite_key_posts)
        |> Ash.Query.filter(id == ^post1.id and serial == ^post1.serial)
        |> Api.read!()

      assert Api.reload!(author) == Api.reload!(fetched_post.author)
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

      author =
        Author
        |> Changeset.new()
        |> Changeset.manage_relationship(
          :composite_key_posts,
          [
            %{id: post1.id, serial: post1.serial},
            post2
          ],
          type: :append_and_remove
        )
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
        |> Changeset.manage_relationship(
          :composite_key_posts,
          [
            %{id: post1.id, serial: post1.serial},
            post2,
            invalid_post
          ],
          type: :append_and_remove
        )

      assert [%Ash.Error.Changes.InvalidRelationship{} = relation_error] = changeset.errors
      assert relation_error.message =~ "Cannot provide structs that don't match the destination"
    end

    test "it returns error if relationship does not exists" do
      post1 = Post |> Changeset.new(%{title: "foo"}) |> Api.create!()

      changeset =
        Author
        |> Changeset.new()
        |> Changeset.manage_relationship(:na, post1, type: :append_and_remove)

      assert %{} == changeset.relationships
      assert [%Ash.Error.Changes.NoSuchRelationship{}] = changeset.errors
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
        |> Changeset.manage_relationship(:posts, [post], type: :append_and_remove)

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

    test "arguments can be provided as strings" do
      Category
      |> Changeset.new(%{"name" => "foo"})
      |> Changeset.set_argument("confirm_name", "foo")
      |> Api.create!(action: :create_with_confirmation)
    end

    test "arguments can be used in invalid changes" do
      assert_raise Ash.Error.Invalid, ~r/Confirmation did not match value/, fn ->
        Category
        |> Changeset.new(%{"name" => "foo"})
        |> Changeset.set_argument(:confirm_name, "bar")
        |> Api.create!(action: :create_with_confirmation)
      end
    end

    test "required arguments can't be nil" do
      assert_raise Ash.Error.Invalid, ~r/argument confirm_name is required/, fn ->
        Category
        |> Changeset.new(%{"name" => "foo"})
        |> Api.create!(action: :create_with_confirmation)
      end
    end

    test "optional arguments should use the default" do
      changeset =
        Category
        |> Changeset.for_create(:create_with_confirmation)

      assert Changeset.get_argument(changeset, :true_optional_argument) == true
      assert Changeset.get_argument(changeset, :false_optional_argument) == false
    end
  end

  describe "for_<action>" do
    test "arguments are validated" do
      assert [
               %Ash.Error.Changes.InvalidAttribute{
                 class: :invalid,
                 field: :confirm_name,
                 message: "Confirmation did not match value",
                 path: []
               }
             ] =
               Ash.Changeset.for_create(Category, :create_with_confirmation, %{
                 "name" => "foo",
                 "confirm_name" => "bar"
               }).errors
    end
  end
end
