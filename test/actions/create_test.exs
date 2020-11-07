defmodule Ash.Test.Actions.CreateTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule Authorized do
    use Ash.Resource,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Test.Authorizer]

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key :id
      attribute(:name, :string)
    end

    actions do
      read(:default)
      create(:default)
      update(:default)
    end
  end

  defmodule Profile do
    @moduledoc false
    use Ash.Resource,
      data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      read(:default)
      create(:default)
      update(:default)
    end

    attributes do
      attribute(:id, :uuid, primary_key?: true, default: &Ecto.UUID.generate/0)
      attribute(:bio, :string)
      attribute(:date, :date)
    end

    relationships do
      belongs_to(:author, Ash.Test.Actions.CreateTest.Author)
    end
  end

  defmodule DuplicateName do
    use Ash.Resource.Change

    def change(changeset, _, _) do
      case Ash.Changeset.fetch_change(changeset, :name) do
        :error -> changeset
        {:ok, name} -> Ash.Changeset.change_attribute(changeset, :name, name <> name)
      end
    end
  end

  defmodule Author do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      read :default
      create :default, primary?: true

      create :only_allow_name do
        accept([:name])
      end

      create :duplicate_name do
        change {DuplicateName, []}
      end

      update :default
    end

    attributes do
      attribute(:id, :uuid, primary_key?: true, default: &Ecto.UUID.generate/0)
      attribute(:name, :string)
      attribute(:bio, :string)
    end

    relationships do
      has_one(:profile, Profile, destination_field: :author_id)

      has_many(:posts, Ash.Test.Actions.CreateTest.Post, destination_field: :author_id)
    end
  end

  defmodule PostDefaults do
    @moduledoc false
    def garbage2, do: "garbage2"
    def garbage3, do: "garbage3"
  end

  defmodule PostLink do
    @moduledoc false
    use Ash.Resource,
      data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      read(:default)

      create(:default)
      update(:default)
    end

    relationships do
      belongs_to(:source_post, Ash.Test.Actions.CreateTest.Post, primary_key?: true)
      belongs_to(:destination_post, Ash.Test.Actions.CreateTest.Post, primary_key?: true)
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      read(:default)
      create(:default)
      update(:default)
    end

    attributes do
      attribute(:id, :uuid, primary_key?: true, default: &Ecto.UUID.generate/0)
      attribute(:title, :string)
      attribute(:contents, :string)
      attribute(:tag, :string, default: "garbage")
      attribute(:tag2, :string, default: &PostDefaults.garbage2/0)
      attribute(:tag3, :string, default: {PostDefaults, :garbage3, []})
      attribute(:list_attribute, {:array, :integer})
      attribute(:date, :date)

      attribute(:list_attribute_with_constraints, {:array, :integer},
        constraints: [
          min_length: 2,
          max_length: 10,
          items: [min: -10, max: 10]
        ]
      )
    end

    relationships do
      belongs_to(:author, Author)

      many_to_many(:related_posts, __MODULE__,
        through: PostLink,
        source_field_on_join_table: :source_post_id,
        destination_field_on_join_table: :destination_post_id
      )
    end
  end

  defmodule Api do
    @moduledoc false
    use Ash.Api

    resources do
      resource(Author)
      resource(Post)
      resource(Profile)
      resource(PostLink)
      resource(Authorized)
    end
  end

  import Ash.Changeset

  describe "simple creates" do
    test "allows creating a record with valid attributes" do
      assert %Post{title: "foo", contents: "bar"} =
               Post
               |> new()
               |> change_attributes(%{
                 title: "foo",
                 contents: "bar",
                 date: Date.utc_today()
               })
               |> Api.create!()
    end

    test "constant default values are set properly" do
      assert %Post{tag: "garbage"} =
               Post
               |> new()
               |> change_attribute(:title, "foo")
               |> Api.create!()
    end

    test "constant functions values are set properly" do
      assert %Post{tag2: "garbage2"} =
               Post
               |> new()
               |> change_attribute(:title, "foo")
               |> Api.create!()
    end

    test "constant module/function values are set properly" do
      assert %Post{tag3: "garbage3"} =
               Post
               |> new()
               |> change_attribute(:title, "foo")
               |> Api.create!()
    end
  end

  describe "accept" do
    test "allows using attributes in the list" do
      Author
      |> new()
      |> change_attribute(:name, "fred")
      |> Api.create!(action: :only_allow_name)
    end

    test "it prevents using attributes not in the list" do
      assert_raise Ash.Error.Invalid, ~r/Invalid value provided for bio: cannot be changed/, fn ->
        Author
        |> new()
        |> change_attribute(:bio, "foo")
        |> Api.create!(action: :only_allow_name)
      end
    end
  end

  describe "changeset" do
    test "changes are run properly" do
      author =
        Author
        |> new(%{name: "fred"})
        |> Api.create!(action: :duplicate_name)

      assert author.name == "fredfred"
    end
  end

  describe "creating many to many relationships" do
    test "allows creating with a many_to_many relationship" do
      post2 =
        Post
        |> new()
        |> change_attribute(:title, "title2")
        |> Api.create!()

      post3 =
        Post
        |> new()
        |> change_attribute(:title, "title3")
        |> Api.create!()

      Post
      |> new()
      |> replace_relationship(:related_posts, [post2, post3])
      |> Api.create!()
    end

    test "it updates the join table properly" do
      post2 =
        Post
        |> new()
        |> change_attribute(:title, "title2")
        |> Api.create!()

      post3 =
        Post
        |> new()
        |> change_attribute(:title, "title3")
        |> Api.create!()

      Post
      |> new()
      |> replace_relationship(:related_posts, [post2, post3])
      |> Api.create!()

      assert [_, _] =
               PostLink
               |> Ash.Query.new()
               |> Api.read!()
    end

    test "it responds with the relationship filled in" do
      post2 =
        Post
        |> new()
        |> change_attribute(:title, "title2")
        |> Api.create!()

      post3 =
        Post
        |> new()
        |> change_attribute(:title, "title3")
        |> Api.create!()

      post =
        Post
        |> new()
        |> replace_relationship(:related_posts, [post2, post3])
        |> Api.create!()

      assert Enum.sort(post.related_posts) ==
               Enum.sort([
                 Api.get!(Post, post2.id),
                 Api.get!(Post, post3.id)
               ])
    end
  end

  describe "creating with has_one relationships" do
    test "allows creating with has_one relationship" do
      profile =
        Profile
        |> new()
        |> change_attribute(:bio, "best dude")
        |> Api.create!()

      Author
      |> new()
      |> change_attribute(:name, "fred")
      |> replace_relationship(:profile, profile)
    end

    test "it sets the relationship on the destination record accordingly" do
      profile =
        Profile
        |> new()
        |> change_attribute(:bio, "best dude")
        |> Api.create!()

      author =
        Author
        |> new()
        |> change_attribute(:name, "fred")
        |> replace_relationship(:profile, profile)
        |> Api.create!()

      assert Api.get!(Profile, profile.id).author_id == author.id
    end

    test "it responds with the relationship filled in" do
      profile =
        Profile
        |> new()
        |> change_attribute(:bio, "best dude")
        |> Api.create!()

      author =
        Author
        |> new()
        |> change_attribute(:name, "fred")
        |> replace_relationship(:profile, profile)
        |> Api.create!()

      assert author.profile.author_id == author.id
    end
  end

  describe "creating with a has_many relationship" do
    test "allows creating with a has_many relationship" do
      post =
        Post
        |> new()
        |> change_attribute(:title, "sup")
        |> Api.create!()

      Author
      |> new()
      |> change_attribute(:name, "foobar")
      |> replace_relationship(:posts, [post])
      |> Api.create!()
    end
  end

  describe "creating with belongs_to relationships" do
    test "allows creating with belongs_to relationship" do
      author =
        Author
        |> new()
        |> change_attribute(:bio, "best dude")
        |> Api.create!()

      Post
      |> new()
      |> change_attribute(:title, "foobar")
      |> replace_relationship(:author, author)
      |> Api.create!()
    end

    test "it sets the relationship on the destination record accordingly" do
      author =
        Author
        |> new()
        |> change_attribute(:bio, "best dude")
        |> Api.create!()

      post =
        Post
        |> new()
        |> change_attribute(:title, "foobar")
        |> replace_relationship(:author, author)
        |> Api.create!()

      assert Api.get!(Post, post.id).author_id == author.id
    end

    test "it responds with the relationship field filled in" do
      author =
        Author
        |> new()
        |> change_attribute(:bio, "best dude")
        |> Api.create!()

      post =
        Post
        |> new()
        |> change_attribute(:title, "foobar")
        |> replace_relationship(:author, author)
        |> Api.create!()

      assert post.author_id == author.id
    end

    test "it responds with the relationship filled in" do
      author =
        Author
        |> new()
        |> change_attribute(:bio, "best dude")
        |> Api.create!()

      post =
        Post
        |> new()
        |> change_attribute(:title, "foobar")
        |> replace_relationship(:author, author)
        |> Api.create!()

      assert post.author == author
    end
  end

  describe "list type" do
    test "it can store a list" do
      assert Post
             |> new()
             |> change_attribute(:list_attribute, [1, 2, 3, 4])
             |> Api.create!()
    end
  end

  describe "list type constraints" do
    test "it honors min_length" do
      assert_raise Ash.Error.Invalid, ~r/must have more than 2 items/, fn ->
        Post
        |> new()
        |> change_attribute(:list_attribute_with_constraints, [])
        |> Api.create!()
      end
    end

    test "it honors max_length" do
      assert_raise Ash.Error.Invalid, ~r/must have fewer than 10 items/, fn ->
        list = [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]

        Post
        |> new()
        |> change_attribute(:list_attribute_with_constraints, list)
        |> Api.create!()
      end
    end

    test "it honors item constraints" do
      assert_raise Ash.Error.Invalid, ~r/must be less than 10 at index 0/, fn ->
        list = [28, 2, 4]

        Post
        |> new()
        |> change_attribute(:list_attribute_with_constraints, list)
        |> Api.create!()
      end
    end
  end

  describe "unauthorized create" do
    test "it does not create the record" do
      assert_raise(Ash.Error.Forbidden, fn ->
        Authorized
        |> new()
        |> change_attribute(:name, "foo")
        |> Api.create!(authorize?: true)
      end)

      assert [] = Api.read!(Authorized)
    end
  end
end
