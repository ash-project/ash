# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Actions.CreateTest do
  @moduledoc false
  require Ash.Flags
  use ExUnit.Case, async: true

  import Ash.Test
  import Ash.Expr, only: [expr: 1]

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
      defaults [:read, :destroy, create: :*, update: :*]
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
      defaults [:read, :destroy, create: :*, update: :*]
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
      default_accept [:bio, :date]
      defaults [:read, :destroy, :create, :update]
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
      default_accept [:bio, :name]
      defaults [:read, :destroy, :create, :update]

      create :only_allow_name do
        accept([:name])
      end

      create :duplicate_name do
        change {DuplicateName, []}
      end

      create :create_with_no_accepts do
        accept [:bio]
      end

      create :testing do
        change fn changeset, _ ->
          raise "Uh oh!"
        end
      end

      create :required_error_and_other_errors do
        argument :required, :string, allow_nil?: false

        change fn changeset, _ ->
          Ash.Changeset.add_error(
            changeset,
            Ash.Error.Changes.InvalidChanges.exception(
              fields: [:required],
              message: "Invalid for some other reason"
            )
          )
        end
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
      attribute(:private_name, :string)
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
      defaults [:read, :destroy, create: :*, update: :*]

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
      defaults [:read, :destroy, create: :*, update: :*]
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

  defmodule AtomicOnlyValidation do
    use Ash.Resource.Validation

    @impl true
    def atomic(_, _, _) do
      :ok
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets, domain: Ash.Test.Domain

    ets do
      private?(true)
    end

    actions do
      default_accept [
        :list_attribute_with_constraints,
        :required_boolean_with_default,
        :required_with_default,
        :binary,
        :date,
        :list_attribute,
        :tag3,
        :tag2,
        :tag,
        :contents,
        :title
      ]

      defaults [:read, :destroy, :create, :update]

      create :create_with_required do
        require_attributes [:tag]
      end

      create :create_with_private_argument do
        argument :private_name, :string, allow_nil?: false, public?: false
        accept [:title]

        change set_attribute(:private_name, arg(:private_name))
      end

      create :create_with_nested_array_argument do
        argument :array_of_names, {:array, {:array, :string}}
      end

      create :create_with_set_attribute do
        accept [:title, :tag]
        change set_attribute(:tag, "not_default", new?: true)
      end

      create :with_atomic_only_validations do
        validate AtomicOnlyValidation
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
      attribute(:private_name, :string)

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

    identities do
      identity :unique_title, :title do
        pre_check_with Ash.Test.Actions.BulkCreateTest.Domain
      end
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

  defmodule PostRequiringAuthorId do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets, domain: Ash.Test.Domain

    ets do
      private?(true)
    end

    actions do
      default_accept [:author_id, :title]
      defaults [:read, :destroy, create: :*, update: :*]
    end

    attributes do
      uuid_primary_key :id
      attribute(:title, :string, allow_nil?: false, public?: true)

      timestamps()
    end

    relationships do
      belongs_to(:author, Author, public?: true, allow_nil?: false)
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
      defaults [:read, :destroy, create: :*, update: :*]
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
      defaults [:read, :destroy, create: :*, update: :*]

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

  defmodule Tenant do
    @doc false
    use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, :create, :update, :destroy]
    end

    attributes do
      uuid_primary_key :id, writable?: true
    end

    defimpl Ash.ToTenant do
      def to_tenant(tenant, _resource), do: tenant.id
    end
  end

  defmodule MultitenantTagLink do
    @moduledoc false
    use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    multitenancy do
      strategy :attribute
      attribute :tenant_id
    end

    attributes do
      attribute :type, :string do
        public?(true)
      end
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    relationships do
      belongs_to :tenant, Tenant, allow_nil?: false, primary_key?: true

      belongs_to :source_tag, Ash.Test.Actions.CreateTest.MultitenantTag,
        primary_key?: true,
        allow_nil?: false,
        public?: true

      belongs_to :destination_tag, Ash.Test.Actions.CreateTest.MultitenantTag,
        primary_key?: true,
        allow_nil?: false,
        public?: true
    end
  end

  defmodule MultitenantTag do
    @moduledoc false
    use Ash.Resource,
      domain: Ash.Test.Domain,
      data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    multitenancy do
      strategy :attribute
      attribute :tenant_id
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]

      create :create_with_related_tags do
        argument :related_tags, {:array, :string} do
          allow_nil? false
          constraints min_length: 1, items: [min_length: 1]
        end

        change manage_relationship(:related_tags,
                 on_lookup: :relate,
                 on_no_match: :create,
                 value_is_key: :name,
                 use_identities: [:name]
               )
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string, allow_nil?: false, public?: true

      timestamps()
    end

    relationships do
      belongs_to :tenant, Tenant, allow_nil?: false

      many_to_many :related_tags, __MODULE__,
        through: MultitenantTagLink,
        source_attribute_on_join_resource: :source_tag_id,
        destination_attribute_on_join_resource: :destination_tag_id,
        public?: true
    end

    identities do
      identity :name, [:name], pre_check_with: Ash.Test.Domain
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

    # 4.0 this test should pass, see the set attribute change comment
    # test "create with set_attribute(new?: true) uses tag value from set_attribute" do
    #   assert %Post{title: "some title", tag: "not_default"} =
    #            Post
    #            |> Ash.Changeset.for_create(:create_with_set_attribute, %{title: "some title"})
    #            |> Ash.create!()
    # end

    test "create with set_attribute(new?: true) uses tag override value" do
      assert %Post{title: "something", tag: "existing"} =
               Post
               |> Ash.Changeset.for_create(:create_with_set_attribute, %{
                 title: "something",
                 tag: "existing"
               })
               |> Ash.create!()
    end

    test "allows setting private arguments" do
      assert %Post{title: "title"} =
               Post
               |> Ash.Changeset.for_create(:create_with_private_argument, %{title: "title"},
                 private_arguments: %{private_name: "private"}
               )
               |> Ash.create!()
    end

    test "allows upserting a record" do
      assert %Post{id: id, title: "foo", updated_at: updated_at} =
               Post
               |> Ash.Changeset.new()
               |> Ash.Changeset.change_attributes(%{
                 title: "foo",
                 tag: "v1"
               })
               |> Ash.create!()

      assert %Post{id: ^id, title: "foo", contents: "bar", tag: tag, updated_at: new_updated_at} =
               Post
               |> Ash.Changeset.new()
               |> Ash.Changeset.change_attributes(%{
                 title: "foo",
                 contents: "bar"
               })
               |> Ash.create!(
                 upsert?: true,
                 upsert_identity: :unique_title,
                 upsert_fields: [:contents, :tag, :updated_at]
               )

      refute updated_at == new_updated_at
      # tag not set (but included in upsert_fields) so it reverts to default
      assert tag == "garbage"
    end

    test "allows upserting a record (without using a changeset)" do
      assert %Post{id: id, title: "foo", updated_at: updated_at} =
               Post
               |> Ash.Changeset.new()
               |> Ash.Changeset.change_attributes(%{
                 title: "foo",
                 contents: "bar",
                 tag: "baz"
               })
               |> Ash.create!()

      assert %Post{id: ^id, title: "foo", contents: "bar", tag: tag, updated_at: new_updated_at} =
               Ash.create!(
                 Post,
                 %{
                   title: "foo",
                   contents: "ignored"
                 },
                 upsert?: true,
                 upsert_identity: :unique_title,
                 upsert_fields: [:tag, :updated_at]
               )

      refute updated_at == new_updated_at
      # tag not set (but included in upsert_fields) so it reverts to default
      assert tag == "garbage"
    end

    test "skips upsert when condition doesn't match" do
      assert %Post{id: id, title: "foo", updated_at: updated_at} =
               Post
               |> Ash.Changeset.new()
               |> Ash.Changeset.change_attributes(%{
                 title: "foo",
                 contents: "bar",
                 tag: "before"
               })
               |> Ash.create!()

      assert %Post{id: ^id, title: "foo", contents: "bar", updated_at: ^updated_at} =
               post =
               Post
               |> Ash.Changeset.new()
               |> Ash.Changeset.change_attributes(%{
                 title: "foo",
                 contents: "bar",
                 tag: "after"
               })
               |> Ash.create!(
                 upsert?: true,
                 return_skipped_upsert?: true,
                 upsert_identity: :unique_title,
                 upsert_fields: [:contents, :updated_at],
                 upsert_condition: expr(contents != upsert_conflict(:contents))
               )

      assert Ash.Resource.get_metadata(post, :upsert_skipped)
    end

    test "skips upsert when condition doesn't match, returning error unless asked for" do
      Post
      |> Ash.Changeset.new()
      |> Ash.Changeset.change_attributes(%{
        title: "foo",
        contents: "bar",
        tag: "before"
      })
      |> Ash.create!()

      assert_raise Ash.Error.Invalid, ~r/Stale/, fn ->
        Post
        |> Ash.Changeset.new()
        |> Ash.Changeset.change_attributes(%{
          title: "foo",
          contents: "bar",
          tag: "after"
        })
        |> Ash.create!(
          upsert?: true,
          upsert_identity: :unique_title,
          upsert_fields: [:contents, :updated_at],
          upsert_condition: expr(contents != upsert_conflict(:contents))
        )
      end
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

    test "required errors are omitted when there are other errors" do
      {:error, err} =
        Author
        |> Ash.Changeset.for_create(:required_error_and_other_errors, %{})
        |> Ash.create()

      assert %Ash.Error.Invalid{
               class: :invalid,
               errors: [
                 %{
                   fields: [:required],
                   message: "Invalid for some other reason"
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

    test "a default being set and then overridden will no longer be annotated as a default" do
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

  describe "load" do
    test "allows loading has_many relationship on the changeset" do
      post1 = Ash.create!(Post, %{title: "Post 1"})
      post2 = Ash.create!(Post, %{title: "Post 2"})

      load_query =
        Post
        |> Ash.Query.sort(title: :asc)
        |> Ash.Query.select([:title])

      author =
        Author
        |> Ash.Changeset.new()
        |> Ash.Changeset.change_attribute(:name, "Author")
        |> Ash.Changeset.manage_relationship(:posts, [post2, post1], type: :append_and_remove)
        |> Ash.Changeset.load(posts: load_query)
        |> Ash.create!()

      assert [%Post{title: "Post 1"}, %Post{title: "Post 2"}] = author.posts
    end

    test "allows loading has_many relationship on the action options" do
      post1 = Ash.create!(Post, %{title: "Post 1"})
      post2 = Ash.create!(Post, %{title: "Post 2"})

      load_query =
        Post
        |> Ash.Query.sort(title: :asc)
        |> Ash.Query.select([:title])

      author =
        Author
        |> Ash.Changeset.new()
        |> Ash.Changeset.change_attribute(:name, "Author")
        |> Ash.Changeset.manage_relationship(:posts, [post2, post1], type: :append_and_remove)
        |> Ash.create!(load: [posts: load_query])

      assert [%Post{title: "Post 1"}, %Post{title: "Post 2"}] = author.posts
    end

    test "allows loading paginated has_many relationship on the changeset" do
      post1 = Ash.create!(Post, %{title: "Post 1"})
      post2 = Ash.create!(Post, %{title: "Post 2"})

      offset_pagination_query =
        Post
        |> Ash.Query.sort(title: :asc)
        |> Ash.Query.select([:title])
        |> Ash.Query.page(count: true, limit: 1)

      author =
        Author
        |> Ash.Changeset.new()
        |> Ash.Changeset.change_attribute(:name, "Author")
        |> Ash.Changeset.manage_relationship(:posts, [post2, post1], type: :append_and_remove)
        |> Ash.Changeset.load(posts: offset_pagination_query)
        |> Ash.create!()

      assert %Ash.Page.Keyset{
               results: [%Post{title: "Post 1", __metadata__: %{keyset: keyset}}],
               limit: 1,
               count: 2,
               more?: true
             } = author.posts

      keyset_pagination_query =
        Post
        |> Ash.Query.sort(title: :asc)
        |> Ash.Query.select([:title])
        |> Ash.Query.page(count: true, limit: 1, after: keyset)

      author =
        Author
        |> Ash.Changeset.new()
        |> Ash.Changeset.change_attribute(:name, "Author")
        |> Ash.Changeset.manage_relationship(:posts, [post2, post1], type: :append_and_remove)
        |> Ash.Changeset.load(posts: keyset_pagination_query)
        |> Ash.create!()

      assert %Ash.Page.Keyset{
               results: [%Post{title: "Post 2"}],
               limit: 1,
               count: 2,
               more?: false,
               before: nil,
               after: ^keyset
             } = author.posts
    end

    test "allows loading paginated has_many relationship on the action options" do
      post1 = Ash.create!(Post, %{title: "Post 1"})
      post2 = Ash.create!(Post, %{title: "Post 2"})

      offset_pagination_query =
        Post
        |> Ash.Query.sort(title: :asc)
        |> Ash.Query.select([:title])
        |> Ash.Query.page(count: true, limit: 1)

      author =
        Author
        |> Ash.Changeset.new()
        |> Ash.Changeset.change_attribute(:name, "Author")
        |> Ash.Changeset.manage_relationship(:posts, [post2, post1], type: :append_and_remove)
        |> Ash.create!(load: [posts: offset_pagination_query])

      assert %Ash.Page.Keyset{
               results: [%Post{title: "Post 1", __metadata__: %{keyset: keyset}}],
               limit: 1,
               count: 2,
               more?: true
             } = author.posts

      keyset_pagination_query =
        Post
        |> Ash.Query.sort(title: :asc)
        |> Ash.Query.select([:title])
        |> Ash.Query.page(count: true, limit: 1, after: keyset)

      author =
        Author
        |> Ash.Changeset.new()
        |> Ash.Changeset.change_attribute(:name, "Author")
        |> Ash.Changeset.manage_relationship(:posts, [post2, post1], type: :append_and_remove)
        |> Ash.create!(load: [posts: keyset_pagination_query])

      assert %Ash.Page.Keyset{
               results: [%Post{title: "Post 2"}],
               limit: 1,
               count: 2,
               more?: false,
               before: nil,
               after: ^keyset
             } = author.posts
    end

    test "allows loading many_to_many relationship on the changeset" do
      related_post1 = Ash.create!(Post, %{title: "Related 1"})
      related_post2 = Ash.create!(Post, %{title: "Related 2"})

      load_query =
        Post
        |> Ash.Query.sort(title: :asc)
        |> Ash.Query.select([:title])

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "Post"})
        |> Ash.Changeset.manage_relationship(:related_posts, [related_post2, related_post1],
          type: :append_and_remove
        )
        |> Ash.Changeset.load(related_posts: load_query)
        |> Ash.create!()

      assert [%Post{title: "Related 1"}, %Post{title: "Related 2"}] = post.related_posts
    end

    test "allows loading many_to_many relationship on the action options" do
      related_post1 = Ash.create!(Post, %{title: "Related 1"})
      related_post2 = Ash.create!(Post, %{title: "Related 2"})

      load_query =
        Post
        |> Ash.Query.sort(title: :asc)
        |> Ash.Query.select([:title])

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "Post"})
        |> Ash.Changeset.manage_relationship(:related_posts, [related_post2, related_post1],
          type: :append_and_remove
        )
        |> Ash.create!(load: [related_posts: load_query])

      assert [%Post{title: "Related 1"}, %Post{title: "Related 2"}] = post.related_posts
    end

    test "allows loading paginated many_to_many relationship on the changeset" do
      related_post1 = Ash.create!(Post, %{title: "Related 1"})
      related_post2 = Ash.create!(Post, %{title: "Related 2"})

      offset_pagination_query =
        Post
        |> Ash.Query.sort(title: :asc)
        |> Ash.Query.select([:title])
        |> Ash.Query.page(count: true, limit: 1)

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "Post"})
        |> Ash.Changeset.manage_relationship(:related_posts, [related_post2, related_post1],
          type: :append_and_remove
        )
        |> Ash.Changeset.load(related_posts: offset_pagination_query)
        |> Ash.create!()

      assert %Ash.Page.Keyset{
               results: [%Post{title: "Related 1", __metadata__: %{keyset: keyset}}],
               limit: 1,
               count: 2,
               more?: true
             } = post.related_posts

      keyset_pagination_query =
        Post
        |> Ash.Query.sort(title: :asc)
        |> Ash.Query.select([:title])
        |> Ash.Query.page(count: true, limit: 1, after: keyset)

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "Post"})
        |> Ash.Changeset.manage_relationship(:related_posts, [related_post2, related_post1],
          type: :append_and_remove
        )
        |> Ash.Changeset.load(related_posts: keyset_pagination_query)
        |> Ash.create!(upsert?: true, upsert_fields: [:title], upsert_identity: :unique_title)

      assert %Ash.Page.Keyset{
               results: [%Post{title: "Related 2"}],
               limit: 1,
               count: 2,
               more?: false,
               before: nil,
               after: ^keyset
             } = post.related_posts
    end

    test "allows loading paginated many_to_many relationship on the action options" do
      related_post1 = Ash.create!(Post, %{title: "Related 1"})
      related_post2 = Ash.create!(Post, %{title: "Related 2"})

      offset_pagination_query =
        Post
        |> Ash.Query.sort(title: :asc)
        |> Ash.Query.select([:title])
        |> Ash.Query.page(count: true, limit: 1)

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "Post"})
        |> Ash.Changeset.manage_relationship(:related_posts, [related_post2, related_post1],
          type: :append_and_remove
        )
        |> Ash.create!(load: [related_posts: offset_pagination_query])

      assert %Ash.Page.Keyset{
               results: [%Post{title: "Related 1", __metadata__: %{keyset: keyset}}],
               limit: 1,
               count: 2,
               more?: true
             } = post.related_posts

      keyset_pagination_query =
        Post
        |> Ash.Query.sort(title: :asc)
        |> Ash.Query.select([:title])
        |> Ash.Query.page(count: true, limit: 1, after: keyset)

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "Post"})
        |> Ash.Changeset.manage_relationship(:related_posts, [related_post2, related_post1],
          type: :append_and_remove
        )
        |> Ash.create!(
          load: [related_posts: keyset_pagination_query],
          upsert?: true,
          upsert_fields: [:title],
          upsert_identity: :unique_title
        )

      assert %Ash.Page.Keyset{
               results: [%Post{title: "Related 2"}],
               limit: 1,
               count: 2,
               more?: false,
               before: nil,
               after: ^keyset
             } = post.related_posts
    end

    test "allows loading paginated many_to_many relationship for multitenant resources" do
      tenant = Ash.create!(Tenant, %{})
      _ = Ash.create!(MultitenantTag, %{name: "foo"}, tenant: tenant)

      offset_pagination_query =
        MultitenantTag
        |> Ash.Query.sort(name: :asc)
        |> Ash.Query.select([:name])
        |> Ash.Query.page(count: true, limit: 1)

      tag =
        Ash.create!(MultitenantTag, %{name: "tag 1", related_tags: ["foo", "bar"]},
          action: :create_with_related_tags,
          tenant: tenant,
          load: [related_tags: offset_pagination_query, related_tags_join_assoc: []]
        )

      assert %Ash.Page.Keyset{
               results: [%MultitenantTag{name: "bar", __metadata__: %{keyset: keyset}}],
               limit: 1,
               count: 2,
               more?: true
             } = tag.related_tags

      keyset_pagination_query =
        MultitenantTag
        |> Ash.Query.sort(name: :asc)
        |> Ash.Query.select([:name])
        |> Ash.Query.page(count: true, limit: 1, after: keyset)

      tag =
        Ash.create!(MultitenantTag, %{name: "tag 2", related_tags: ["foo", "bar"]},
          action: :create_with_related_tags,
          tenant: tenant,
          load: [related_tags: keyset_pagination_query]
        )

      assert %Ash.Page.Keyset{
               results: [%MultitenantTag{name: "foo"}],
               limit: 1,
               count: 2,
               more?: false,
               before: nil,
               after: ^keyset
             } = tag.related_tags
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

    test "when creating with belongs_to relationship specified as an attribute, the attribute is in the error" do
      assert {:error,
              %Ash.Error.Invalid{errors: [%Ash.Error.Changes.Required{field: :author_id}]}} =
               PostRequiringAuthorId
               |> Ash.Changeset.new()
               |> Ash.Changeset.for_create(:create, %{title: "foobar"})
               |> Ash.create()
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
      |> Ash.Changeset.new()
      |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
      |> Ash.Changeset.for_create(:create)
      |> Ash.create!()
    end

    test "allows creating with the required belongs_to relationship with an on_no_match :create" do
      Author
      |> Ash.Changeset.for_create(:create, bio: "best dude")
      |> Ash.create!()

      ProfileWithBelongsTo
      |> Ash.Changeset.new()
      |> Ash.Changeset.manage_relationship(:author, %{name: "author name"},
        type: :append_and_remove,
        on_no_match: :create,
        on_lookup: :relate,
        on_match: :ignore
      )
      |> Ash.Changeset.for_create(:create)
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

  test "shows an error if an atomic only validation is used in a create" do
    assert_raise Ash.Error.Framework,
                 ~r/Create actions cannot be made atomic/,
                 fn ->
                   Post
                   |> Ash.Changeset.for_create(:with_atomic_only_validations, %{title: "foo"})
                   |> Ash.create!()
                 end
  end
end
