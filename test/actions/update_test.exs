defmodule Ash.Test.Actions.UpdateTest do
  @moduledoc false
  require Ash.Flags
  use ExUnit.Case, async: true

  import Ash.Test
  require Ash.Query
  require Ash.Expr
  alias Ash.Test.AnyApi, as: Api

  defmodule Authorized do
    @moduledoc false
    use Ash.Resource,
      api: Api,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Test.Authorizer]

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string
    end

    actions do
      defaults [:read, :create, :update, :destroy]
    end
  end

  defmodule Profile do
    @moduledoc false
    use Ash.Resource, api: Api, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      defaults [:create, :read, :update, :destroy]

      update :set_private_attribute_to_nil do
        change set_attribute(:non_nil_private, nil)
      end

      update :set_private_attribute_from_arg do
        argument :private, :string
        change set_attribute(:private, arg(:private))
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :bio, :string, allow_nil?: false
      attribute :non_nil_private, :string, allow_nil?: false, default: "non_nil"
      attribute :private, :string, default: "non_nil"
    end

    relationships do
      belongs_to :author, Ash.Test.Actions.UpdateTest.Author
    end
  end

  defmodule DuplicateName do
    @moduledoc false
    use Ash.Resource.Change

    def change(changeset, _, _) do
      case Ash.Changeset.fetch_change(changeset, :name) do
        :error -> changeset
        {:ok, name} -> Ash.Changeset.change_attribute(changeset, :name, name <> name)
      end
    end
  end

  defmodule ManualUpdateAuthor do
    @moduledoc false
    use Ash.Resource.Change

    def change(changeset, _, _) do
      Ash.Changeset.after_action(changeset, fn _changeset, data ->
        {:ok,
         data
         |> Ash.Changeset.new()
         |> Ash.Changeset.change_attribute(:name, "manual")
         |> Api.update!()}
      end)
    end
  end

  defmodule Author do
    @moduledoc false
    use Ash.Resource, api: Api, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      defaults [:create, :read, :update, :destroy]

      update :only_allow_name do
        accept([:name])
      end

      update :with_validation do
        accept([:name])

        validate attribute_equals(:name, "fred")
        validate compare(:score, greater_than_or_equal_to: 0, less_than_or_equal_to: 10)
      end

      update :duplicate_name do
        change {DuplicateName, []}
      end

      update :manual_update do
        accept []

        manual fn changeset, _ ->
          {:ok,
           changeset.data
           |> Ash.Changeset.for_update(:update, changeset.attributes)
           |> Ash.Changeset.force_change_attribute(:name, "manual")
           |> Api.update!()}
        end
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string
      attribute :bio, :string
      attribute :score, :integer
    end

    relationships do
      has_one :profile, Profile, destination_attribute: :author_id

      has_many :posts, Ash.Test.Actions.UpdateTest.Post, destination_attribute: :author_id
    end
  end

  defmodule PostLink do
    @moduledoc false
    use Ash.Resource, api: Api, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    attributes do
      attribute :type, :string
    end

    actions do
      defaults [:read, :create, :update, :destroy]
    end

    relationships do
      belongs_to :source_post, Ash.Test.Actions.UpdateTest.Post,
        primary_key?: true,
        allow_nil?: false

      belongs_to :destination_post, Ash.Test.Actions.UpdateTest.Post,
        primary_key?: true,
        allow_nil?: false
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource, api: Api, data_layer: Ash.DataLayer.Ets

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

      many_to_many :related_posts, __MODULE__,
        through: PostLink,
        source_attribute_on_join_resource: :source_post_id,
        destination_attribute_on_join_resource: :destination_post_id
    end
  end

  defmodule PaginatedPrimaryRead do
    use Ash.Resource,
      api: Api,
      data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
    end

    actions do
      defaults [:create, :update, :destroy]

      read :read do
        primary? true
        pagination offset?: true, required?: true
      end
    end
  end

  describe "simple updates" do
    test "allows updating a record with valid attributes" do
      post =
        Post
        |> Ash.Changeset.new(%{title: "foo", contents: "bar"})
        |> Api.create!()

      assert %Post{title: "bar", contents: "foo"} =
               post |> Ash.Changeset.new(%{title: "bar", contents: "foo"}) |> Api.update!()
    end
  end

  describe "manual updates" do
    test "the update occurs properly" do
      author =
        Author
        |> Ash.Changeset.new(%{name: "auto"})
        |> Api.create!()

      assert %Author{name: "manual"} =
               author |> Ash.Changeset.new() |> Api.update!(action: :manual_update)
    end
  end

  describe "allow_nil?" do
    test "it does not allow updating a value to `nil` when `allow_nil?: false`" do
      profile =
        Profile
        |> Ash.Changeset.new(%{bio: "foobar"})
        |> Api.create!()

      assert_raise Ash.Error.Invalid, ~r/attribute bio is required/, fn ->
        profile |> Ash.Changeset.new(%{bio: ""}) |> Api.update!()
      end
    end

    test "it does not allow updating a private attribute's value to `nil` when `allow_nil?: false`" do
      profile =
        Profile
        |> Ash.Changeset.new(%{bio: "foobar"})
        |> Api.create!()

      assert_raise Ash.Error.Invalid, ~r/attribute non_nil_private is required/, fn ->
        profile
        |> Ash.Changeset.new(%{bio: "foobar"})
        |> Api.update!(action: :set_private_attribute_to_nil)
      end
    end

    test "it passes through an argument's value" do
      profile =
        Profile
        |> Ash.Changeset.new(%{bio: "foobar"})
        |> Api.create!()

      profile =
        profile
        |> Ash.Changeset.new(%{bio: "foobar"})
        |> Ash.Changeset.for_update(:set_private_attribute_from_arg, %{private: "blah"})
        |> Api.update!()

      assert profile.private == "blah"
    end
  end

  describe "select" do
    test "allows selecting fields on the changeset" do
      post =
        Post
        |> Ash.Changeset.new(%{title: "foo", contents: "bar"})
        |> Api.create!()

      assert %Post{title: "bar", contents: %Ash.NotLoaded{}} =
               post
               |> Ash.Changeset.new(%{title: "bar", contents: "foo"})
               |> Ash.Changeset.select(:title)
               |> Api.update!()
    end
  end

  describe "allow" do
    test "allows attributes in the list" do
      author =
        Author
        |> Ash.Changeset.new(%{name: "fred"})
        |> Api.create!()

      author
      |> Ash.Changeset.new(%{name: "joe"})
      |> Api.update!(action: :only_allow_name)
    end

    test "does not allow attributes in the list" do
      author =
        Author
        |> Ash.Changeset.new(%{name: "fred"})
        |> Api.create!()

      assert_raise Ash.Error.Invalid, ~r/Invalid value provided for bio: cannot be changed/, fn ->
        author
        |> Ash.Changeset.new(%{bio: "bio"})
        |> Api.update!(action: :only_allow_name)
      end
    end
  end

  describe "atomics" do
    test "atomics can be added to a changeset" do
      author =
        Author
        |> Ash.Changeset.new(%{name: "fred"})
        |> Api.create!()

      author =
        author
        |> Ash.Changeset.for_update(:only_allow_name)
        |> Ash.Changeset.atomic_update(:name, Ash.Expr.expr(name <> " weasley"))
        |> Api.update!()

      assert author.name == "fred weasley"
    end

    test "a changeset can be fully atomic" do
      changeset =
        Ash.Changeset.fully_atomic_changeset(Author, :with_validation, %{name: "fred weasly"},
          eager?: false
        )

      assert changeset.valid?
      assert changeset.atomics[:name]
    end
  end

  describe "changeset" do
    test "changes are run properly" do
      author =
        Author
        |> Ash.Changeset.new(%{name: "fred"})
        |> Api.create!()

      author =
        author
        |> Ash.Changeset.new(%{name: "joe"})
        |> Api.update!(action: :duplicate_name)

      assert author.name == "joejoe"
    end
  end

  describe "updating many to many relationships" do
    test "allows updating with a many_to_many relationship" do
      post =
        Post
        |> Ash.Changeset.new(%{title: "title"})
        |> Api.create!()

      post2 =
        Post
        |> Ash.Changeset.new(%{title: "title2"})
        |> Api.create!()

      post3 =
        Post
        |> Ash.Changeset.new(%{title: "title3"})
        |> Api.create!()

      post
      |> Ash.Changeset.new()
      |> Ash.Changeset.manage_relationship(:related_posts, [post2, post3],
        type: :append_and_remove
      )
      |> Api.update!()
    end

    test "allows directly managing a many_to_many relationship" do
      post =
        Post
        |> Ash.Changeset.new(%{title: "title"})
        |> Ash.Changeset.manage_relationship(:related_posts, [%{title: "title0"}],
          type: :direct_control
        )
        |> Api.create!()

      other_post = Post |> Ash.Query.filter(title == "title0") |> Api.read_one!()

      post
      |> Ash.Changeset.new()
      |> Ash.Changeset.manage_relationship(
        :related_posts,
        [%{title: "title3", id: other_post.id}, %{title: "title1"}],
        type: :direct_control
      )
      |> Api.update!()

      assert ["title", "title1", "title3"] =
               Post |> Ash.Query.sort(:title) |> Api.read!() |> Enum.map(& &1.title)
    end

    test "it updates the join resource properly" do
      post =
        Post
        |> Ash.Changeset.new(%{title: "title"})
        |> Api.create!()

      post2 =
        Post
        |> Ash.Changeset.new(%{title: "title2"})
        |> Api.create!()

      post3 =
        Post
        |> Ash.Changeset.new(%{title: "title3"})
        |> Api.create!()

      post
      |> Ash.Changeset.new()
      |> Ash.Changeset.manage_relationship(:related_posts, [post2, post3],
        type: :append_and_remove
      )
      |> Api.update!()

      assert [_, _] = Api.read!(PostLink)
    end

    test "it responds with the relationship filled in" do
      post =
        Post
        |> Ash.Changeset.new(%{title: "title"})
        |> Api.create!()

      post2 =
        Post
        |> Ash.Changeset.new(%{title: "title2"})
        |> Api.create!()

      post3 =
        Post
        |> Ash.Changeset.new(%{title: "title3"})
        |> Api.create!()

      new_post =
        post
        |> Ash.Changeset.new()
        |> Ash.Changeset.manage_relationship(:related_posts, [post2, post3],
          type: :append_and_remove
        )
        |> Api.update!()

      assert Enum.sort(strip_metadata(new_post.related_posts)) ==
               Enum.sort([
                 Api.get!(Post, post2.id),
                 Api.get!(Post, post3.id)
               ])
               |> strip_metadata()
    end

    test "it updates any join fields" do
      post =
        Post
        |> Ash.Changeset.new(%{title: "title"})
        |> Api.create!()

      post2 =
        Post
        |> Ash.Changeset.new(%{title: "title2"})
        |> Api.create!()

      post3 =
        Post
        |> Ash.Changeset.new(%{title: "title3"})
        |> Api.create!()

      new_post =
        post
        |> Ash.Changeset.new()
        |> Ash.Changeset.manage_relationship(
          :related_posts,
          [
            Ash.Resource.set_metadata(post2, %{join_keys: %{type: "a"}}),
            Ash.Resource.set_metadata(post3, %{join_keys: %{type: "b"}})
          ],
          type: :append_and_remove
        )
        |> Api.update!()
        |> Api.load!(:related_posts_join_assoc)

      types = Enum.sort(Enum.map(new_post.related_posts_join_assoc, &Map.get(&1, :type)))

      assert types == ["a", "b"]

      new_post =
        new_post
        |> Ash.Changeset.new()
        |> Ash.Changeset.manage_relationship(
          :related_posts,
          [
            Ash.Resource.set_metadata(post2, %{join_keys: %{type: "c"}}),
            Ash.Resource.set_metadata(post3, %{join_keys: %{type: "d"}})
          ],
          type: :append_and_remove,
          on_match: :update,
          on_lookup: :relate
        )
        |> Api.update!()
        |> Api.load!(:related_posts_join_assoc)

      types = Enum.sort(Enum.map(new_post.related_posts_join_assoc, &Map.get(&1, :type)))

      assert types == ["c", "d"]
    end
  end

  describe "updating with has_one relationships" do
    test "allows updating with has_one relationship" do
      profile =
        Profile
        |> Ash.Changeset.new(%{bio: "best dude"})
        |> Api.create!()

      profile2 =
        Profile
        |> Ash.Changeset.new(%{bio: "second best dude"})
        |> Api.create!()

      author =
        Author
        |> Ash.Changeset.new(%{name: "fred"})
        |> Ash.Changeset.manage_relationship(:profile, profile, type: :append_and_remove)
        |> Api.create!()

      author
      |> Ash.Changeset.new()
      |> Ash.Changeset.manage_relationship(:profile, profile2, type: :append_and_remove)
      |> Api.update!()
    end

    test "it sets the relationship on the destination record accordingly" do
      profile =
        Profile
        |> Ash.Changeset.new(%{bio: "best dude"})
        |> Api.create!()

      profile2 =
        Profile
        |> Ash.Changeset.new(%{bio: "second best dude"})
        |> Api.create!()

      author =
        Author
        |> Ash.Changeset.new(%{name: "fred"})
        |> Ash.Changeset.manage_relationship(:profile, profile, type: :append_and_remove)
        |> Api.create!()

      author
      |> Ash.Changeset.new()
      |> Ash.Changeset.manage_relationship(:profile, profile2, type: :append_and_remove)
      |> Api.update!()

      assert Api.get!(Profile, profile.id).author_id == nil
      assert Api.get!(Profile, profile2.id).author_id == author.id
    end

    test "it responds with the relationship filled in" do
      profile =
        Profile
        |> Ash.Changeset.new(%{bio: "best dude"})
        |> Api.create!()

      profile2 =
        Profile
        |> Ash.Changeset.new(%{bio: "second best dude"})
        |> Api.create!()

      author =
        Author
        |> Ash.Changeset.new(%{name: "fred"})
        |> Ash.Changeset.manage_relationship(:profile, profile, type: :append_and_remove)
        |> Api.create!()

      updated_author =
        author
        |> Ash.Changeset.new()
        |> Ash.Changeset.manage_relationship(:profile, profile2, type: :append_and_remove)
        |> Api.update!()

      assert %{updated_author.profile | __metadata__: nil} == %{
               profile2
               | author_id: author.id,
                 __metadata__: nil
             }
    end
  end

  describe "updating with a has_many relationship" do
    test "allows updating with a has_many relationship" do
      post =
        Post
        |> Ash.Changeset.new(%{title: "sup"})
        |> Api.create!()

      post2 =
        Post
        |> Ash.Changeset.new(%{title: "sup2"})
        |> Api.create!()

      author =
        Author
        |> Ash.Changeset.new(%{name: "foobar"})
        |> Ash.Changeset.manage_relationship(:posts, [post], type: :append_and_remove)
        |> Api.create!()

      author
      |> Ash.Changeset.new()
      |> Ash.Changeset.manage_relationship(:posts, [post, post2], type: :append_and_remove)
      |> Api.update!()
    end

    test "it sets the relationship on the destination records accordingly" do
      post =
        Post
        |> Ash.Changeset.new(%{title: "sup"})
        |> Api.create!()

      post2 =
        Post
        |> Ash.Changeset.new(%{title: "sup2"})
        |> Api.create!()

      author =
        Author
        |> Ash.Changeset.new(%{name: "foobar"})
        |> Ash.Changeset.manage_relationship(:posts, [post], type: :append_and_remove)
        |> Api.create!()

      author =
        author
        |> Ash.Changeset.new()
        |> Ash.Changeset.manage_relationship(:posts, [post2.id], type: :append_and_remove)
        |> Api.update!()

      assert Api.get!(Post, post.id).author_id == nil
      assert Api.get!(Post, post2.id).author_id == author.id
    end

    test "it responds with the relationship field filled in" do
      post =
        Post
        |> Ash.Changeset.new(%{title: "sup"})
        |> Api.create!()

      post2 =
        Post
        |> Ash.Changeset.new(%{title: "sup2"})
        |> Api.create!()

      author =
        Author
        |> Ash.Changeset.new(%{name: "foobar"})
        |> Ash.Changeset.manage_relationship(:posts, [post], type: :append_and_remove)
        |> Api.create!()

      updated_author =
        author
        |> Ash.Changeset.new()
        |> Ash.Changeset.manage_relationship(:posts, [post2], type: :append_and_remove)
        |> Api.update!()

      post = Api.get!(Post, post2.id)

      assert Enum.map(updated_author.posts, &%{&1 | __metadata__: nil}) == [
               %{post | __metadata__: nil}
             ]
    end
  end

  describe "updating with belongs_to relationships" do
    test "allows updating with belongs_to relationship" do
      author =
        Author
        |> Ash.Changeset.new(%{name: "best dude"})
        |> Api.create!()

      author2 =
        Author
        |> Ash.Changeset.new(%{name: "best dude2"})
        |> Api.create!()

      post =
        Post
        |> Ash.Changeset.new(%{title: "foobar"})
        |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
        |> Api.create!()

      post
      |> Ash.Changeset.new()
      |> Ash.Changeset.manage_relationship(:author, author2, type: :append_and_remove)
      |> Api.update!()
    end

    test "sets the relationship on the destination records accordingly" do
      author =
        Author
        |> Ash.Changeset.new(%{name: "best dude"})
        |> Api.create!()

      author2 =
        Author
        |> Ash.Changeset.new(%{name: "best dude2"})
        |> Api.create!()

      post =
        Post
        |> Ash.Changeset.new(%{title: "foobar"})
        |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
        |> Api.create!()

      post
      |> Ash.Changeset.new()
      |> Ash.Changeset.manage_relationship(:author, author2, type: :append_and_remove)
      |> Api.update!()

      author2 = Api.get!(Author, author2.id, load: :posts)

      assert Enum.map(author2.posts, & &1.id) == [
               post.id
             ]
    end

    test "it responds with the relationship field filled in" do
      author =
        Author
        |> Ash.Changeset.new(%{name: "best dude"})
        |> Api.create!()

      author2 =
        Author
        |> Ash.Changeset.new(%{name: "best dude2"})
        |> Api.create!()

      post =
        Post
        |> Ash.Changeset.new(%{title: "foobar"})
        |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
        |> Api.create!()

      updated_post =
        post
        |> Ash.Changeset.new()
        |> Ash.Changeset.manage_relationship(:author, author2, type: :append_and_remove)
        |> Api.update!()

      assert updated_post.author.id ==
               Api.get!(Author, author2.id).id
    end
  end

  describe "unauthorized update" do
    test "it does not update the record" do
      record =
        Authorized
        |> Ash.Changeset.new(%{name: "bar"})
        |> Api.create!()

      start_supervised({Ash.Test.Authorizer, check: :forbidden, strict_check: :continue})

      assert_raise(Ash.Error.Forbidden, fn ->
        record
        |> Ash.Changeset.new(%{name: "foo"})
        |> Api.update!(authorize?: true)
      end)

      assert Api.get!(Authorized, record.id, authorize?: false).name == "bar"
    end
  end
end
