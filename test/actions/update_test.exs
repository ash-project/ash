defmodule Ash.Test.Actions.UpdateTest do
  @moduledoc false
  require Ash.Flags
  use ExUnit.Case, async: true

  import Ash.Test
  require Ash.Query
  require Ash.Expr
  alias Ash.Test.Domain, as: Domain

  defmodule Authorized do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Test.Authorizer]

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string, public?: true
    end

    actions do
      default_accept :*
      defaults [:read, :create, :update, :destroy]
    end
  end

  defmodule Profile do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:create, :read, :update, :destroy]

      update :set_private_attribute_to_nil do
        accept []
        change set_attribute(:non_nil_private, nil)
      end

      update :set_private_attribute_from_arg do
        argument :private, :string
        change set_attribute(:private, arg(:private))
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :bio, :string, allow_nil?: false, public?: true
      attribute :non_nil_private, :string, allow_nil?: false, default: "non_nil", public?: true
      attribute :private, :string, default: "non_nil", public?: true
    end

    relationships do
      belongs_to :author, Ash.Test.Actions.UpdateTest.Author do
        public?(true)
      end
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
         |> Ash.update!()}
      end)
    end
  end

  defmodule Author do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
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
           |> Ash.update!()}
        end
      end
    end

    attributes do
      uuid_primary_key :id

      attribute :name, :string do
        public?(true)
      end

      attribute :bio, :string do
        public?(true)
      end

      attribute :score, :integer do
        public?(true)
      end
    end

    relationships do
      has_one :profile, Profile, destination_attribute: :author_id, public?: true

      has_many :posts, Ash.Test.Actions.UpdateTest.Post,
        destination_attribute: :author_id,
        public?: true
    end
  end

  defmodule PostLink do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    attributes do
      attribute :type, :string do
        public?(true)
      end
    end

    actions do
      default_accept :*
      defaults [:read, :create, :update, :destroy]
    end

    relationships do
      belongs_to :source_post, Ash.Test.Actions.UpdateTest.Post,
        primary_key?: true,
        allow_nil?: false,
        public?: true

      belongs_to :destination_post, Ash.Test.Actions.UpdateTest.Post,
        primary_key?: true,
        allow_nil?: false,
        public?: true
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, :create, :update, :destroy]
    end

    attributes do
      uuid_primary_key :id

      attribute :title, :string do
        public?(true)
      end

      attribute :contents, :string do
        public?(true)
      end
    end

    relationships do
      belongs_to :author, Author, public?: true

      many_to_many :related_posts, __MODULE__,
        through: PostLink,
        source_attribute_on_join_resource: :source_post_id,
        destination_attribute_on_join_resource: :destination_post_id,
        public?: true
    end
  end

  defmodule PaginatedPrimaryRead do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
    end

    actions do
      default_accept :*
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
        |> Ash.Changeset.for_create(:create, %{title: "foo", contents: "bar"})
        |> Ash.create!()

      assert %Post{title: "bar", contents: "foo"} =
               post
               |> Ash.Changeset.for_update(:update, %{title: "bar", contents: "foo"})
               |> Ash.update!()
    end
  end

  describe "manual updates" do
    test "the update occurs properly" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "auto"})
        |> Ash.create!()

      assert %Author{name: "manual"} =
               author
               |> Ash.Changeset.for_update(:update)
               |> Ash.update!(action: :manual_update)
    end
  end

  describe "allow_nil?" do
    test "it does not allow updating a value to `nil` when `allow_nil?: false`" do
      profile =
        Profile
        |> Ash.Changeset.for_create(:create, %{bio: "foobar"})
        |> Ash.create!()

      assert_raise Ash.Error.Invalid, ~r/attribute bio is required/, fn ->
        profile |> Ash.Changeset.for_update(:update, %{bio: ""}) |> Ash.update!()
      end
    end

    test "it does not allow updating a private attribute's value to `nil` when `allow_nil?: false`" do
      profile =
        Profile
        |> Ash.Changeset.for_create(:create, %{bio: "foobar"})
        |> Ash.create!()

      assert_raise Ash.Error.Invalid, ~r/attribute non_nil_private is required/, fn ->
        profile
        |> Ash.Changeset.for_update(:update, %{bio: "foobar"})
        |> Ash.update!(action: :set_private_attribute_to_nil)
      end
    end

    test "it passes through an argument's value" do
      profile =
        Profile
        |> Ash.Changeset.for_create(:create, %{bio: "foobar"})
        |> Ash.create!()

      profile =
        profile
        |> Ash.Changeset.for_update(:set_private_attribute_from_arg, %{private: "blah"})
        |> Ash.update!()

      assert profile.private == "blah"
    end
  end

  describe "select" do
    test "allows selecting fields on the changeset" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "foo", contents: "bar"})
        |> Ash.create!()

      assert %Post{title: "bar", contents: %Ash.NotLoaded{}} =
               post
               |> Ash.Changeset.for_update(:update, %{title: "bar", contents: "foo"})
               |> Ash.Changeset.select(:title)
               |> Ash.update!()
    end
  end

  describe "allow" do
    test "allows attributes in the list" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "fred"})
        |> Ash.create!()

      author
      |> Ash.Changeset.for_update(:update, %{name: "joe"})
      |> Ash.update!(action: :only_allow_name)
    end

    test "does not allow attributes in the list" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "fred"})
        |> Ash.create!()

      assert_raise Ash.Error.Invalid, ~r/Invalid value provided for bio: cannot be changed/, fn ->
        author
        |> Ash.Changeset.for_update(:update, %{bio: "bio"})
        |> Ash.update!(action: :only_allow_name)
      end
    end
  end

  describe "atomics" do
    test "atomics can be added to a changeset" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "fred"})
        |> Ash.create!()

      author =
        author
        |> Ash.Changeset.for_update(:only_allow_name)
        |> Ash.Changeset.atomic_update(:name, Ash.Expr.expr(name <> " weasley"))
        |> Ash.update!()

      assert author.name == "fred weasley"
    end

    test "a changeset can be fully atomic" do
      changeset =
        Ash.Changeset.fully_atomic_changeset(Author, :with_validation, %{name: "fred weasly"},
          eager?: false
        )

      assert changeset.valid?
    end
  end

  describe "changeset" do
    test "changes are run properly" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "fred"})
        |> Ash.create!()

      author =
        author
        |> Ash.Changeset.for_update(:duplicate_name, %{name: "joe"})
        |> Ash.update!()

      assert author.name == "joejoe"
    end
  end

  describe "updating many to many relationships" do
    test "allows updating with a many_to_many relationship" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "title"})
        |> Ash.create!()

      post2 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "title2"})
        |> Ash.create!()

      post3 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "title3"})
        |> Ash.create!()

      post
      |> Ash.Changeset.new()
      |> Ash.Changeset.manage_relationship(:related_posts, [post2, post3],
        type: :append_and_remove
      )
      |> Ash.update!()
    end

    test "allows directly managing a many_to_many relationship" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "title"})
        |> Ash.Changeset.manage_relationship(:related_posts, [%{title: "title0"}],
          type: :direct_control
        )
        |> Ash.create!()

      other_post = Post |> Ash.Query.filter(title == "title0") |> Ash.read_one!()

      post
      |> Ash.Changeset.new()
      |> Ash.Changeset.manage_relationship(
        :related_posts,
        [%{title: "title3", id: other_post.id}, %{title: "title1"}],
        type: :direct_control
      )
      |> Ash.update!()

      assert ["title", "title1", "title3"] =
               Post |> Ash.Query.sort(:title) |> Ash.read!() |> Enum.map(& &1.title)
    end

    test "it updates the join resource properly" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "title"})
        |> Ash.create!()

      post2 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "title2"})
        |> Ash.create!()

      post3 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "title3"})
        |> Ash.create!()

      post
      |> Ash.Changeset.new()
      |> Ash.Changeset.manage_relationship(:related_posts, [post2, post3],
        type: :append_and_remove
      )
      |> Ash.update!()

      assert [_, _] = Ash.read!(PostLink)
    end

    test "it responds with the relationship filled in" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "title"})
        |> Ash.create!()

      post2 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "title2"})
        |> Ash.create!()

      post3 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "title3"})
        |> Ash.create!()

      new_post =
        post
        |> Ash.Changeset.new()
        |> Ash.Changeset.manage_relationship(:related_posts, [post2, post3],
          type: :append_and_remove
        )
        |> Ash.update!()

      assert Enum.sort(strip_metadata(new_post.related_posts)) ==
               Enum.sort([
                 Ash.get!(Post, post2.id),
                 Ash.get!(Post, post3.id)
               ])
               |> strip_metadata()
    end

    test "it updates any join fields" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "title"})
        |> Ash.create!()

      post2 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "title2"})
        |> Ash.create!()

      post3 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "title3"})
        |> Ash.create!()

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
        |> Ash.update!()
        |> Ash.load!(:related_posts_join_assoc)

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
        |> Ash.update!()
        |> Ash.load!(:related_posts_join_assoc)

      types = Enum.sort(Enum.map(new_post.related_posts_join_assoc, &Map.get(&1, :type)))

      assert types == ["c", "d"]
    end
  end

  describe "updating with has_one relationships" do
    test "allows updating with has_one relationship" do
      profile =
        Profile
        |> Ash.Changeset.for_create(:create, %{bio: "best dude"})
        |> Ash.create!()

      profile2 =
        Profile
        |> Ash.Changeset.for_create(:create, %{bio: "second best dude"})
        |> Ash.create!()

      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "fred"})
        |> Ash.Changeset.manage_relationship(:profile, profile, type: :append_and_remove)
        |> Ash.create!()

      author
      |> Ash.Changeset.new()
      |> Ash.Changeset.manage_relationship(:profile, profile2, type: :append_and_remove)
      |> Ash.update!()
    end

    test "it sets the relationship on the destination record accordingly" do
      profile =
        Profile
        |> Ash.Changeset.for_create(:create, %{bio: "best dude"})
        |> Ash.create!()

      profile2 =
        Profile
        |> Ash.Changeset.for_create(:create, %{bio: "second best dude"})
        |> Ash.create!()

      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "fred"})
        |> Ash.Changeset.manage_relationship(:profile, profile, type: :append_and_remove)
        |> Ash.create!()

      author
      |> Ash.Changeset.new()
      |> Ash.Changeset.manage_relationship(:profile, profile2, type: :append_and_remove)
      |> Ash.update!()

      assert Ash.get!(Profile, profile.id).author_id == nil
      assert Ash.get!(Profile, profile2.id).author_id == author.id
    end

    test "it responds with the relationship filled in" do
      profile =
        Profile
        |> Ash.Changeset.for_create(:create, %{bio: "best dude"})
        |> Ash.create!()

      profile2 =
        Profile
        |> Ash.Changeset.for_create(:create, %{bio: "second best dude"})
        |> Ash.create!()

      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "fred"})
        |> Ash.Changeset.manage_relationship(:profile, profile, type: :append_and_remove)
        |> Ash.create!()

      updated_author =
        author
        |> Ash.Changeset.new()
        |> Ash.Changeset.manage_relationship(:profile, profile2, type: :append_and_remove)
        |> Ash.update!()

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
        |> Ash.Changeset.for_create(:create, %{title: "sup"})
        |> Ash.create!()

      post2 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "sup2"})
        |> Ash.create!()

      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "foobar"})
        |> Ash.Changeset.manage_relationship(:posts, [post], type: :append_and_remove)
        |> Ash.create!()

      author
      |> Ash.Changeset.new()
      |> Ash.Changeset.manage_relationship(:posts, [post, post2], type: :append_and_remove)
      |> Ash.update!()
    end

    test "it sets the relationship on the destination records accordingly" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "sup"})
        |> Ash.create!()

      post2 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "sup2"})
        |> Ash.create!()

      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "foobar"})
        |> Ash.Changeset.manage_relationship(:posts, [post], type: :append_and_remove)
        |> Ash.create!()

      author =
        author
        |> Ash.Changeset.new()
        |> Ash.Changeset.manage_relationship(:posts, [post2.id], type: :append_and_remove)
        |> Ash.update!()

      assert Ash.get!(Post, post.id).author_id == nil
      assert Ash.get!(Post, post2.id).author_id == author.id
    end

    test "it responds with the relationship field filled in" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "sup"})
        |> Ash.create!()

      post2 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "sup2"})
        |> Ash.create!()

      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "foobar"})
        |> Ash.Changeset.manage_relationship(:posts, [post], type: :append_and_remove)
        |> Ash.create!()

      updated_author =
        author
        |> Ash.Changeset.new()
        |> Ash.Changeset.manage_relationship(:posts, [post2], type: :append_and_remove)
        |> Ash.update!()

      post = Ash.get!(Post, post2.id)

      assert Enum.map(updated_author.posts, &%{&1 | __metadata__: nil}) == [
               %{post | __metadata__: nil}
             ]
    end
  end

  describe "updating with belongs_to relationships" do
    test "allows updating with belongs_to relationship" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "best dude"})
        |> Ash.create!()

      author2 =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "best dude2"})
        |> Ash.create!()

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "foobar"})
        |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
        |> Ash.create!()

      post
      |> Ash.Changeset.new()
      |> Ash.Changeset.manage_relationship(:author, author2, type: :append_and_remove)
      |> Ash.update!()
    end

    test "sets the relationship on the destination records accordingly" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "best dude"})
        |> Ash.create!()

      author2 =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "best dude2"})
        |> Ash.create!()

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "foobar"})
        |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
        |> Ash.create!()

      post
      |> Ash.Changeset.new()
      |> Ash.Changeset.manage_relationship(:author, author2, type: :append_and_remove)
      |> Ash.update!()

      author2 = Ash.get!(Author, author2.id, load: :posts)

      assert Enum.map(author2.posts, & &1.id) == [
               post.id
             ]
    end

    test "it responds with the relationship field filled in" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "best dude"})
        |> Ash.create!()

      author2 =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "best dude2"})
        |> Ash.create!()

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "foobar"})
        |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
        |> Ash.create!()

      updated_post =
        post
        |> Ash.Changeset.new()
        |> Ash.Changeset.manage_relationship(:author, author2, type: :append_and_remove)
        |> Ash.update!()

      assert updated_post.author.id ==
               Ash.get!(Author, author2.id).id
    end
  end

  describe "unauthorized update" do
    test "it does not update the record" do
      record =
        Authorized
        |> Ash.Changeset.for_create(:create, %{name: "bar"})
        |> Ash.create!()

      start_supervised({Ash.Test.Authorizer, check: :forbidden, strict_check: :continue})

      assert_raise(Ash.Error.Forbidden, fn ->
        record
        |> Ash.Changeset.for_update(:update, %{name: "foo"})
        |> Ash.update!(authorize?: true)
      end)

      assert Ash.get!(Authorized, record.id, authorize?: false).name == "bar"
    end
  end
end
