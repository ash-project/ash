defmodule Ash.Test.Actions.CreateTest do
  @moduledoc false
  require Ash.Flags
  use ExUnit.Case, async: true

  import Ash.Test

  defmodule Authorized do
    @moduledoc false
    use Ash.Resource,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Test.Authorizer],
      domain: Ash.Test.Domain

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key :id
      attribute(:name, :string, public?: true)
    end

    actions do
      default_accept :*
      defaults [:create, :read, :update, :destroy]
    end
  end

  defmodule Profile do
    @moduledoc false
    use Ash.Resource,
      data_layer: Ash.DataLayer.Ets,
      domain: Ash.Test.Domain

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:create, :read, :update, :destroy]
    end

    attributes do
      uuid_primary_key :id
      attribute(:bio, :string, public?: true)
      attribute(:date, :date, public?: true)
    end

    relationships do
      belongs_to :author, Ash.Test.Actions.CreateTest.Author do
        public?(true)
      end
    end
  end

  defmodule ProfileWithBelongsTo do
    @moduledoc false
    use Ash.Resource,
      data_layer: Ash.DataLayer.Ets,
      domain: Ash.Test.Domain

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:create, :read, :update, :destroy]
    end

    attributes do
      uuid_primary_key :id
      attribute(:bio, :string, public?: true)
      attribute(:date, :date, public?: true)
    end

    relationships do
      belongs_to :author, Ash.Test.Actions.CreateTest.Author, allow_nil?: false, public?: true
    end
  end

  defmodule DuplicateName do
    @moduledoc false
    use Ash.Resource.Change

    def change(changeset, _, _) do
      case Ash.Changeset.fetch_change(changeset, :name) do
        :error ->
          changeset

        {:ok, name} ->
          Ash.Changeset.change_attribute(changeset, :name, name <> name)
      end
    end
  end

  defmodule ManualCreateAuthor do
    @moduledoc false
    use Ash.Resource.Change

    def change(changeset, _, _) do
      Ash.Changeset.after_action(changeset, fn _, nil ->
        {:ok,
         Ash.Test.Actions.CreateTest.Author
         |> Ash.Changeset.for_create(:create, %{name: "manual"})
         |> Ash.create!()}
      end)
    end
  end

  defmodule ManualCreateAuthorWithRequiredId do
    @moduledoc false
    use Ash.Resource.Change

    def change(changeset, _, _) do
      Ash.Changeset.after_action(changeset, fn _, nil ->
        {:ok,
         Ash.Test.Actions.CreateTest.AuthorWithRequiredId
         |> Ash.Changeset.for_create(:create, %{name: "manual"})
         |> Ash.Changeset.force_change_attribute(:id, Ash.UUID.generate())
         |> Ash.create!()}
      end)
    end
  end

  defmodule Author do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets, domain: Ash.Test.Domain

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:create, :read, :update, :destroy]

      create :only_allow_name do
        accept([:name])
      end

      create :duplicate_name do
        change {DuplicateName, []}
      end

      create :manual_create do
        manual fn _, _ ->
          {:ok,
           Ash.Test.Actions.CreateTest.Author
           |> Ash.Changeset.for_create(:create, %{name: "manual"})
           |> Ash.create!()}
        end
      end
    end

    attributes do
      uuid_primary_key :id
      attribute(:name, :string, public?: true)
      attribute(:bio, :string, public?: true)
    end

    relationships do
      has_one :profile, Profile, destination_attribute: :author_id, public?: true

      has_many :posts, Ash.Test.Actions.CreateTest.Post,
        destination_attribute: :author_id,
        public?: true
    end
  end

  defmodule AuthorWithRequiredId do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets, domain: Ash.Test.Domain

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:create, :read, :update, :destroy]

      create :manual_create do
        accept []

        manual fn _, _ ->
          {:ok,
           Ash.Test.Actions.CreateTest.AuthorWithRequiredId
           |> Ash.Changeset.for_create(:create, %{name: "manual"})
           |> Ash.Changeset.force_change_attribute(:id, Ash.UUID.generate())
           |> Ash.create!()}
        end
      end
    end

    attributes do
      uuid_primary_key :id, generated?: false, default: nil
      attribute(:name, :string, allow_nil?: false, public?: true)
      attribute(:bio, :string, public?: true)
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
      data_layer: Ash.DataLayer.Ets,
      domain: Ash.Test.Domain

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:create, :read, :update, :destroy]
    end

    relationships do
      belongs_to(:source_post, Ash.Test.Actions.CreateTest.Post,
        primary_key?: true,
        allow_nil?: false,
        public?: true
      )

      belongs_to(:destination_post, Ash.Test.Actions.CreateTest.Post,
        primary_key?: true,
        allow_nil?: false,
        public?: true
      )
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets, domain: Ash.Test.Domain

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:create, :read, :update, :destroy]

      create :create_with_required do
        require_attributes [:tag]
      end

      create :create_with_nested_array_argument do
        argument :array_of_names, {:array, {:array, :string}}
      end
    end

    changes do
      change fn changeset, _ ->
        Ash.Changeset.after_transaction(changeset, fn
          _, {:error, error} ->
            send(self(), {:error, error})

          _, result ->
            result
        end)
      end
    end

    attributes do
      uuid_primary_key :id
      attribute(:title, :string, allow_nil?: false, public?: true)
      attribute(:contents, :string, public?: true)
      attribute(:tag, :string, default: "garbage", public?: true)
      attribute(:tag2, :string, default: &PostDefaults.garbage2/0, public?: true)
      attribute(:tag3, :string, default: {PostDefaults, :garbage3, []}, public?: true)
      attribute(:list_attribute, {:array, :integer}, public?: true)
      attribute(:date, :date, public?: true)
      attribute(:binary, :binary, public?: true)

      attribute(:required_with_default, :string,
        allow_nil?: false,
        default: "string",
        public?: true
      )

      attribute(:required_boolean_with_default, :boolean,
        allow_nil?: false,
        default: false,
        public?: true
      )

      attribute(:list_attribute_with_constraints, {:array, :integer},
        public?: true,
        constraints: [
          min_length: 2,
          max_length: 10,
          items: [min: -10, max: 10]
        ]
      )

      timestamps()
    end

    relationships do
      belongs_to(:author, Author, public?: true)

      many_to_many(:related_posts, __MODULE__,
        through: PostLink,
        source_attribute_on_join_resource: :source_post_id,
        destination_attribute_on_join_resource: :destination_post_id,
        public?: true
      )
    end
  end

  defmodule GeneratedPkey do
    @moduledoc false
    use Ash.Resource,
      data_layer: Ash.DataLayer.Ets,
      domain: Ash.Test.Domain

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:create, :read, :update, :destroy]
    end

    attributes do
      uuid_primary_key :id
    end
  end

  defmodule GlobalValidation do
    @moduledoc false
    use Ash.Resource,
      data_layer: Ash.DataLayer.Ets,
      domain: Ash.Test.Domain

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:create, :read, :update, :destroy]

      create :manual do
        skip_global_validations?(true)

        manual fn changeset, _ ->
          Ash.Changeset.apply_attributes(changeset)
        end
      end
    end

    validations do
      validate compare(:foo, greater_than: 10)
    end

    attributes do
      uuid_primary_key :id

      attribute :foo, :integer do
        public?(true)
        allow_nil? false
      end
    end
  end

  describe "simple creates" do
    test "allows creating a record with valid attributes" do
      assert %Post{title: "foo", contents: "bar"} =
               Post
               |> Ash.Changeset.new()
               |> Ash.Changeset.change_attributes(%{
                 title: "foo",
                 contents: "bar",
                 date: Date.utc_today(),
                 binary: <<0, 1, 2, 3, 4, 5>>
               })
               |> Ash.create!()
    end

    test "timestamps will match each other" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "foobar"})
        |> Ash.create!()

      assert post.inserted_at == post.updated_at
    end

    test "allow_nil validation" do
      {:error, error} =
        Post
        |> Ash.Changeset.for_create(:create, %{})
        |> Ash.create()

      assert %Ash.Error.Invalid{} = error
    end

    test "nested array arguments are accepted" do
      Post
      |> Ash.Changeset.for_create(:create_with_nested_array_argument, %{
        title: "foobar",
        array_of_names: [["foo"], ["bar", "baz"]]
      })
      |> Ash.create!()
    end

    test "allow_nil validation when attribute provided" do
      {:error, error} =
        Post
        |> Ash.Changeset.for_create(:create, %{title: nil})
        |> Ash.create()

      assert %Ash.Error.Invalid{} = error
    end

    test "return missing required attribute" do
      {:error, err} =
        Post
        |> Ash.Changeset.new()
        |> Ash.Changeset.change_attributes(%{
          contents: "bar",
          date: Date.utc_today()
        })
        |> Ash.create()

      assert %Ash.Error.Invalid{
               class: :invalid,
               errors: [
                 %Ash.Error.Changes.Required{
                   class: :invalid,
                   field: :title
                 }
               ]
             } = err
    end

    test "generated fields are not required" do
      assert %GeneratedPkey{} =
               GeneratedPkey
               |> Ash.Changeset.new()
               |> Ash.create!()
    end

    test "constant default values are set properly" do
      assert %Post{tag: "garbage"} =
               Post
               |> Ash.Changeset.new()
               |> Ash.Changeset.change_attribute(:title, "foo")
               |> Ash.create!()
    end

    test "nil will override defaults" do
      post =
        Post
        |> Ash.Changeset.new()
        |> Ash.Changeset.change_attribute(:title, "foo")
        |> Ash.Changeset.change_attribute(:tag, nil)
        |> Ash.create!()

      assert post.tag == nil
    end

    test "a default being set will be annotated as a default" do
      changeset =
        Post
        |> Ash.Changeset.new()
        |> Ash.Changeset.change_attribute(:title, "foo")
        |> Ash.Changeset.for_create(:create)

      assert :tag in changeset.defaults
    end

    test "a default being set and then overriden will no longer be annotated as a default" do
      changeset =
        Post
        |> Ash.Changeset.new()
        |> Ash.Changeset.change_attribute(:title, "foo")
        |> Ash.Changeset.for_create(:create)

      assert :tag in changeset.defaults

      force_changeset = Ash.Changeset.force_change_attribute(changeset, :tag, "foo")
      refute :tag in force_changeset.defaults
    end

    test "nil will error on required attribute with default" do
      assert_raise Ash.Error.Invalid, ~r/required_with_default is required/, fn ->
        Post
        |> Ash.Changeset.new()
        |> Ash.Changeset.change_attribute(:title, "foo")
        |> Ash.Changeset.change_attribute(:tag, nil)
        |> Ash.Changeset.change_attribute(:required_with_default, nil)
        |> Ash.create!()
      end
    end

    test "constant functions values are set properly" do
      assert %Post{tag2: "garbage2"} =
               Post
               |> Ash.Changeset.new()
               |> Ash.Changeset.change_attribute(:title, "foo")
               |> Ash.create!()
    end

    test "constant module/function values are set properly" do
      assert %Post{tag3: "garbage3"} =
               Post
               |> Ash.Changeset.new()
               |> Ash.Changeset.change_attribute(:title, "foo")
               |> Ash.create!()
    end

    test "binary values are set properly" do
      assert %Post{binary: <<0, 1, 2>>} =
               Post
               |> Ash.Changeset.new()
               |> Ash.Changeset.change_attribute(:title, "foo")
               |> Ash.Changeset.change_attribute(:binary, <<0, 1, 2>>)
               |> Ash.create!()
    end
  end

  describe "manual creates" do
    test "the manual action succeeds" do
      Author
      |> Ash.Changeset.for_create(:manual_create)
      |> Ash.create!()

      assert [%{name: "manual"}] = Ash.read!(Author)
    end

    test "the manual action does not require values that aren't accepted" do
      AuthorWithRequiredId
      |> Ash.Changeset.for_create(:manual_create)
      |> Ash.create!()

      assert [%{name: "manual"}] = Ash.read!(AuthorWithRequiredId)
    end
  end

  describe "require_attributes" do
    test "it requires attributes that have a default" do
      assert_raise Ash.Error.Invalid, ~r/attribute tag is required/, fn ->
        Post
        |> Ash.Changeset.for_create(:create_with_required, %{title: "foo"})
        |> Ash.create!()
      end
    end

    test "it does not raise an error when those attributes have been set" do
      Post
      |> Ash.Changeset.for_create(:create_with_required, %{title: "foo", tag: "foo"})
      |> Ash.create!()
    end
  end

  describe "accept" do
    test "allows using attributes in the list" do
      Author
      |> Ash.Changeset.new()
      |> Ash.Changeset.change_attribute(:name, "fred")
      |> Ash.create!(action: :only_allow_name)
    end

    test "it prevents using attributes not in the list" do
      assert_raise Ash.Error.Invalid, ~r/Invalid value provided for bio: cannot be changed/, fn ->
        Author
        |> Ash.Changeset.new()
        |> Ash.Changeset.change_attribute(:bio, "foo")
        |> Ash.create!(action: :only_allow_name)
      end
    end
  end

  describe "changeset" do
    test "changes are run properly" do
      author =
        Author
        |> Ash.Changeset.for_create(:duplicate_name, %{name: "fred"})
        |> Ash.create!()

      assert author.name == "fredfred"
    end
  end

  describe "select" do
    test "allows selecting fields on the changeset" do
      author =
        Author
        |> Ash.Changeset.for_create(:duplicate_name, %{name: "fred"})
        |> Ash.Changeset.select(:bio)
        |> Ash.create!()

      assert %Ash.NotLoaded{field: :name} = author.name
    end
  end

  describe "creating many to many relationships" do
    test "allows creating with a many_to_many relationship" do
      post2 =
        Post
        |> Ash.Changeset.new()
        |> Ash.Changeset.change_attribute(:title, "title2")
        |> Ash.create!()

      post3 =
        Post
        |> Ash.Changeset.new()
        |> Ash.Changeset.change_attribute(:title, "title3")
        |> Ash.create!()

      Post
      |> Ash.Changeset.for_create(:create, %{title: "cannot_be_missing"})
      |> Ash.Changeset.manage_relationship(:related_posts, [post2, post3],
        type: :append_and_remove
      )
      |> Ash.create!()
    end

    test "it updates the join resource properly" do
      post2 =
        Post
        |> Ash.Changeset.new()
        |> Ash.Changeset.change_attribute(:title, "title2")
        |> Ash.Changeset.for_create(:create)
        |> Ash.create!()

      post3 =
        Post
        |> Ash.Changeset.new()
        |> Ash.Changeset.change_attribute(:title, "title3")
        |> Ash.create!()

      Post
      |> Ash.Changeset.for_create(:create, %{title: "title4"})
      |> Ash.Changeset.manage_relationship(:related_posts, [post2, post3],
        type: :append_and_remove
      )
      |> Ash.create!()

      assert [_, _] =
               PostLink
               |> Ash.Query.new()
               |> Ash.read!()
    end

    test "it responds with the relationship filled in" do
      post2 =
        Post
        |> Ash.Changeset.new()
        |> Ash.Changeset.change_attribute(:title, "title2")
        |> Ash.create!()
        |> strip_metadata()

      post3 =
        Post
        |> Ash.Changeset.new()
        |> Ash.Changeset.change_attribute(:title, "title3")
        |> Ash.create!()
        |> strip_metadata()

      post =
        Post
        |> Ash.Changeset.new()
        |> Ash.Changeset.change_attributes(%{title: "cannot_be_missing"})
        |> Ash.Changeset.manage_relationship(:related_posts, [post2, post3],
          type: :append_and_remove
        )
        |> Ash.create!()
        |> strip_metadata()

      assert post.related_posts ==
               [
                 Ash.get!(Post, post2.id),
                 Ash.get!(Post, post3.id)
               ]
               |> strip_metadata()
    end
  end

  describe "creating with has_one relationships" do
    test "allows creating with has_one relationship" do
      profile =
        Profile
        |> Ash.Changeset.new()
        |> Ash.Changeset.change_attribute(:bio, "best dude")
        |> Ash.create!()

      Author
      |> Ash.Changeset.new()
      |> Ash.Changeset.change_attribute(:name, "fred")
      |> Ash.Changeset.manage_relationship(:profile, profile, type: :append_and_remove)
    end

    test "it sets the relationship on the destination record accordingly" do
      profile =
        Profile
        |> Ash.Changeset.new()
        |> Ash.Changeset.change_attribute(:bio, "best dude")
        |> Ash.create!()

      author =
        Author
        |> Ash.Changeset.new()
        |> Ash.Changeset.change_attribute(:name, "fred")
        |> Ash.Changeset.manage_relationship(:profile, profile, type: :append_and_remove)
        |> Ash.create!()

      assert Ash.get!(Profile, profile.id).author_id == author.id
    end

    test "it responds with the relationship filled in" do
      profile =
        Profile
        |> Ash.Changeset.new()
        |> Ash.Changeset.change_attribute(:bio, "best dude")
        |> Ash.create!()

      author =
        Author
        |> Ash.Changeset.new()
        |> Ash.Changeset.change_attribute(:name, "fred")
        |> Ash.Changeset.manage_relationship(:profile, profile, type: :append_and_remove)
        |> Ash.create!()

      assert author.profile.author_id == author.id
    end
  end

  describe "creating with a has_many relationship" do
    test "allows creating with a has_many relationship" do
      post =
        Post
        |> Ash.Changeset.new()
        |> Ash.Changeset.change_attribute(:title, "sup")
        |> Ash.create!()

      Author
      |> Ash.Changeset.new()
      |> Ash.Changeset.change_attribute(:name, "foobar")
      |> Ash.Changeset.manage_relationship(:posts, [post], type: :append_and_remove)
      |> Ash.create!()
    end
  end

  describe "creating with belongs_to relationships" do
    test "allows creating with belongs_to relationship" do
      author =
        Author
        |> Ash.Changeset.new()
        |> Ash.Changeset.change_attribute(:bio, "best dude")
        |> Ash.create!()

      Post
      |> Ash.Changeset.new()
      |> Ash.Changeset.change_attribute(:title, "foobar")
      |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
      |> Ash.create!()
    end

    test "it sets the relationship on the destination record accordingly" do
      author =
        Author
        |> Ash.Changeset.new()
        |> Ash.Changeset.change_attribute(:bio, "best dude")
        |> Ash.create!()

      post =
        Post
        |> Ash.Changeset.new()
        |> Ash.Changeset.change_attribute(:title, "foobar")
        |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
        |> Ash.create!()

      assert Ash.get!(Post, post.id).author_id == author.id
    end

    test "it responds with the relationship field filled in" do
      author =
        Author
        |> Ash.Changeset.new()
        |> Ash.Changeset.change_attribute(:bio, "best dude")
        |> Ash.create!()

      post =
        Post
        |> Ash.Changeset.new()
        |> Ash.Changeset.change_attribute(:title, "foobar")
        |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
        |> Ash.create!()

      assert post.author_id == author.id
    end

    test "it responds with the relationship filled in" do
      author =
        Author
        |> Ash.Changeset.new()
        |> Ash.Changeset.change_attribute(:bio, "best dude")
        |> Ash.create!()

      post =
        Post
        |> Ash.Changeset.new()
        |> Ash.Changeset.change_attribute(:title, "foobar")
        |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
        |> Ash.create!()

      assert post.author.id == author.id
    end

    test "it clears the relationship if replaced with nil" do
      author =
        Author
        |> Ash.Changeset.new()
        |> Ash.Changeset.change_attribute(:bio, "best dude")
        |> Ash.create!()

      post =
        Post
        |> Ash.Changeset.new()
        |> Ash.Changeset.change_attribute(:title, "foobar")
        |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
        |> Ash.create!()

      post =
        post
        |> Ash.Changeset.new()
        |> Ash.Changeset.change_attribute(:title, "foobuz")
        |> Ash.Changeset.manage_relationship(:author, nil, type: :append_and_remove)
        |> Ash.update!()

      assert post.author == nil
      assert post.author_id == nil
    end
  end

  describe "creating with required belongs_to relationships" do
    test "does not allow creating without the required belongs_to relationship" do
      assert_raise Ash.Error.Invalid, ~r/relationship author is required/, fn ->
        ProfileWithBelongsTo
        |> Ash.Changeset.for_create(:create)
        |> Ash.create!()
      end
    end

    test "allows creating with the required belongs_to relationship" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, bio: "best dude")
        |> Ash.create!()

      ProfileWithBelongsTo
      |> Ash.Changeset.for_create(:create)
      |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
      |> Ash.create!()
    end

    test "allows creating with the required belongs_to relationship with an on_no_match :create" do
      Author
      |> Ash.Changeset.for_create(:create, bio: "best dude")
      |> Ash.create!()

      ProfileWithBelongsTo
      |> Ash.Changeset.for_create(:create)
      |> Ash.Changeset.manage_relationship(:author, %{name: "author name"},
        type: :append_and_remove,
        on_no_match: :create,
        on_lookup: :relate,
        on_match: :ignore
      )
      |> Ash.create!()
    end
  end

  describe "list type" do
    test "it can store a list" do
      assert Post
             |> Ash.Changeset.new()
             |> Ash.Changeset.change_attribute(:list_attribute, [1, 2, 3, 4])
             |> Ash.Changeset.for_create(:create, %{title: "cannot_be_missing"})
             |> Ash.create!()
    end
  end

  describe "list type constraints" do
    test "it honors min_length" do
      assert_raise Ash.Error.Invalid, ~r/must have 2 or more items/, fn ->
        Post
        |> Ash.Changeset.new()
        |> Ash.Changeset.change_attribute(:list_attribute_with_constraints, [])
        |> Ash.create!()
      end
    end

    test "it honors max_length" do
      assert_raise Ash.Error.Invalid, ~r/must have 10 or fewer items/, fn ->
        list = [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]

        Post
        |> Ash.Changeset.new()
        |> Ash.Changeset.change_attribute(:list_attribute_with_constraints, list)
        |> Ash.create!()
      end
    end

    test "it honors item constraints" do
      assert_raise Ash.Error.Invalid, ~r/must be less than or equal to 10/, fn ->
        list = [28, 2, 4]

        Post
        |> Ash.Changeset.new()
        |> Ash.Changeset.change_attribute(:list_attribute_with_constraints, list)
        |> Ash.create!()
      end
    end
  end

  describe "unauthorized create" do
    test "it does not create the record" do
      start_supervised({Ash.Test.Authorizer, check: :forbidden, strict_check: :continue})

      assert_raise(Ash.Error.Forbidden, fn ->
        Authorized
        |> Ash.Changeset.new()
        |> Ash.Changeset.change_attribute(:name, "foo")
        |> Ash.create!(authorize?: true)
      end)

      assert [] = Ash.read!(Authorized, authorize?: false)
    end
  end

  describe "global validations" do
    test "they can be skipped" do
      assert %{errors: [%Ash.Error.Changes.InvalidAttribute{field: :foo}]} =
               GlobalValidation
               |> Ash.Changeset.for_create(:create, %{foo: 5})

      assert %{errors: []} =
               GlobalValidation
               |> Ash.Changeset.for_create(:manual, %{foo: 5})
    end
  end

  test "after action hooks are run even on invalid input changesets" do
    assert {:error, _error} =
             Post
             |> Ash.Changeset.for_create(:create, %{title: %{}})
             |> Ash.create()

    assert_receive {:error, _error}
  end
end
