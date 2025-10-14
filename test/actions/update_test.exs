# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Actions.UpdateTest do
  @moduledoc false
  require Ash.Flags
  use ExUnit.Case, async: false

  import Ash.Test
  require Ash.Query
  require Ash.Expr
  alias Ash.Test.Domain, as: Domain

  defmodule AtomicOnlyValidation do
    use Ash.Resource.Validation

    @impl true
    def atomic(_, _, _) do
      :ok
    end
  end

  defmodule Authorized do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Test.Authorizer]

    validations do
      validate AtomicOnlyValidation, on: [:update]
    end

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string, public?: true
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
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
      defaults [:read, :destroy, create: :*, update: :*]

      update :check_non_selected_attr do
        action_select []
        require_atomic? false

        change fn changeset, _ ->
          Ash.Changeset.after_action(changeset, fn _changeset, result ->
            case result.bio do
              %Ash.NotLoaded{} ->
                {:ok, result}

              value ->
                raise "Should have been not loaded: #{inspect(value)}"
            end
          end)
        end
      end

      update :set_nilable do
        accept [:nilable]
        require_atomic? false
        atomic_upgrade? true
        require_attributes [:nilable]
      end

      update :set_private_attribute_to_nil do
        accept []
        change set_attribute(:non_nil_private, nil, set_when_nil?: true)
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
      attribute :nilable, :string, public?: true
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
      defaults [:read, :destroy, create: :*, update: [:name, :score]]

      update :only_allow_name do
        accept([:name])
      end

      update :with_validation do
        accept([:name])

        validate attribute_equals(:name, "fred")
        validate compare(:score, greater_than_or_equal_to: 0, less_than_or_equal_to: 10)
      end

      update :with_partially_atomic_validation do
        accept([:name])

        argument :match?, :boolean do
          allow_nil? false
        end

        validate match(:name, "[a-z]+") do
          where argument_equals(:match?, true)
        end
      end

      update :duplicate_name do
        require_atomic? false
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
      defaults [:read, :destroy, create: :*, update: :*]
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
      defaults [:read, :destroy, create: :*, update: :*]
    end

    attributes do
      uuid_primary_key :id

      attribute :title, :string do
        public?(true)
      end

      attribute :contents, :string do
        public?(true)
      end

      attribute :score, :decimal do
        public? true
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
      defaults [:destroy, create: :*, update: :*]

      read :read do
        primary? true
        pagination offset?: true, required?: true
      end
    end
  end

  defmodule Tenant do
    @doc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

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
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

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

      belongs_to :source_tag, Ash.Test.Actions.UpdateTest.MultitenantTag,
        primary_key?: true,
        allow_nil?: false,
        public?: true

      belongs_to :destination_tag, Ash.Test.Actions.UpdateTest.MultitenantTag,
        primary_key?: true,
        allow_nil?: false,
        public?: true
    end
  end

  defmodule MultitenantTag do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
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

      update :add_related_tags do
        require_atomic? false

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
      identity :name, [:name], pre_check_with: Domain
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

  describe "updating unavailable data" do
    test "can handle updating a not loaded field that has a custom equality check" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{
          title: "foo",
          contents: "bar",
          score: Decimal.new("0")
        })
        |> Ash.create!()
        |> Map.put(:score, %Ash.NotLoaded{})

      assert %Post{title: "bar", contents: "foo", score: score} =
               post
               |> Ash.Changeset.for_update(:update, %{
                 title: "bar",
                 contents: "foo",
                 score: Decimal.new("1")
               })
               |> Ash.update!()

      assert Decimal.equal?(score, Decimal.new("1"))
    end
  end

  describe "updating forbidden data" do
    test "can handle updating a forbidden field that has a custom equality check" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{
          title: "foo",
          contents: "bar",
          score: Decimal.new("0")
        })
        |> Ash.create!()
        |> Map.put(:score, %Ash.ForbiddenField{})

      assert %Post{title: "bar", contents: "foo", score: score} =
               post
               |> Ash.Changeset.for_update(:update, %{
                 title: "bar",
                 contents: "foo",
                 score: Decimal.new("1")
               })
               |> Ash.update!()

      assert Decimal.equal?(score, Decimal.new("1"))
    end
  end

  describe "require_attributes" do
    test "it prevents setting the attribute to `nil`" do
      profile =
        Profile
        |> Ash.Changeset.for_create(:create, %{bio: "foobar"})
        |> Ash.create!()

      assert_raise Ash.Error.Invalid, ~r/nilable is required/, fn ->
        profile
        |> Ash.Changeset.for_update(:set_nilable, %{nilable: nil})
        |> Ash.update!()
      end

      assert_raise Ash.Error.Invalid, ~r/nilable is required/, fn ->
        profile
        |> Ash.Changeset.for_update(:set_nilable, %{nilable: nil})
        |> Ash.update!(atomic_upgrade?: false)
      end
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

  # describe "action_select" do
  #   test "it is applied before hooks are run" do
  #     profile =
  #       Profile
  #       |> Ash.Changeset.for_create(:create, %{bio: "foobar"})
  #       |> Ash.create!()

  #     profile
  #     |> Ash.Changeset.for_update(:check_non_selected_attr, %{bio: "bio"})
  #     |> Ash.Changeset.select([])
  #     |> Ash.update!()
  #   end
  # end

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

  describe "load" do
    test "should not unload already loaded relationships when load option is not provided" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "Name"})
        |> Ash.create!()

      Post
      |> Ash.Changeset.for_create(:create, %{title: "Post 1"})
      |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
      |> Ash.create!()

      author = author |> Ash.load!(:posts)

      assert [%Post{}] = author.posts

      author =
        author
        |> Ash.Changeset.for_update(:update, %{name: "Updated Name"})
        |> Ash.update!()

      assert [%Post{}] = author.posts
    end

    test "allows loading has_many relationship on the changeset" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "Name"})
        |> Ash.create!()

      for n <- [2, 1] do
        Post
        |> Ash.Changeset.for_create(:create, %{title: "Post #{n}"})
        |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
        |> Ash.create!()
      end

      load_query =
        Post
        |> Ash.Query.sort(title: :asc)
        |> Ash.Query.select([:title])

      author =
        author
        |> Ash.Changeset.for_update(:update, %{name: "Updated Name"})
        |> Ash.Changeset.load(posts: load_query)
        |> Ash.update!()

      assert [%Post{title: "Post 1"}, %Post{title: "Post 2"}] = author.posts
    end

    test "allows loading has_many relationship on the action options" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "Name"})
        |> Ash.create!()

      for n <- [2, 1] do
        Post
        |> Ash.Changeset.for_create(:create, %{title: "Post #{n}"})
        |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
        |> Ash.create!()
      end

      load_query =
        Post
        |> Ash.Query.sort(title: :asc)
        |> Ash.Query.select([:title])

      author = Ash.update!(author, %{name: "Updated Name"}, load: [posts: load_query])

      assert [%Post{title: "Post 1"}, %Post{title: "Post 2"}] = author.posts
    end

    test "allows loading paginated has_many relationship on the changeset" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "Name"})
        |> Ash.create!()

      for n <- [2, 1] do
        Post
        |> Ash.Changeset.for_create(:create, %{title: "Post #{n}"})
        |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
        |> Ash.create!()
      end

      offset_pagination_query =
        Post
        |> Ash.Query.sort(title: :asc)
        |> Ash.Query.select([:title])
        |> Ash.Query.page(count: true, limit: 1)

      author =
        author
        |> Ash.Changeset.for_update(:update, %{name: "Updated Name 1"})
        |> Ash.Changeset.load(posts: offset_pagination_query)
        |> Ash.update!()

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
        author
        |> Ash.Changeset.for_update(:update, %{name: "Updated Name 2"})
        |> Ash.Changeset.load(posts: keyset_pagination_query)
        |> Ash.update!()

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
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "Name"})
        |> Ash.create!()

      for n <- [2, 1] do
        Post
        |> Ash.Changeset.for_create(:create, %{title: "Post #{n}"})
        |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
        |> Ash.create!()
      end

      offset_pagination_query =
        Post
        |> Ash.Query.sort(title: :asc)
        |> Ash.Query.select([:title])
        |> Ash.Query.page(count: true, limit: 1)

      author =
        Ash.update!(author, %{name: "Updated Name 1"}, load: [posts: offset_pagination_query])

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
        Ash.update!(author, %{name: "Updated Name 1"}, load: [posts: keyset_pagination_query])

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

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "Title"})
        |> Ash.Changeset.manage_relationship(:related_posts, [related_post2, related_post1],
          type: :append_and_remove
        )
        |> Ash.create!()

      load_query =
        Post
        |> Ash.Query.sort(title: :asc)
        |> Ash.Query.select([:title])

      post =
        post
        |> Ash.Changeset.for_update(:update, %{title: "Updated Title"})
        |> Ash.Changeset.load(related_posts: load_query)
        |> Ash.update!()

      assert [%Post{title: "Related 1"}, %Post{title: "Related 2"}] = post.related_posts
    end

    test "allows loading many_to_many relationship on the action options" do
      related_post1 = Ash.create!(Post, %{title: "Related 1"})
      related_post2 = Ash.create!(Post, %{title: "Related 2"})

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "Title"})
        |> Ash.Changeset.manage_relationship(:related_posts, [related_post2, related_post1],
          type: :append_and_remove
        )
        |> Ash.create!()

      load_query =
        Post
        |> Ash.Query.sort(title: :asc)
        |> Ash.Query.select([:title])

      post = Ash.update!(post, %{title: "Updated Title"}, load: [related_posts: load_query])

      assert [%Post{title: "Related 1"}, %Post{title: "Related 2"}] = post.related_posts
    end

    test "allows loading paginated many_to_many relationship on the changeset" do
      related_post1 = Ash.create!(Post, %{title: "Related 1"})
      related_post2 = Ash.create!(Post, %{title: "Related 2"})

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "Title"})
        |> Ash.Changeset.manage_relationship(:related_posts, [related_post2, related_post1],
          type: :append_and_remove
        )
        |> Ash.create!()

      offset_pagination_query =
        Post
        |> Ash.Query.sort(title: :asc)
        |> Ash.Query.select([:title])
        |> Ash.Query.page(count: true, limit: 1)

      post =
        post
        |> Ash.Changeset.for_update(:update, %{title: "Updated Title 1"})
        |> Ash.Changeset.load(related_posts: offset_pagination_query)
        |> Ash.update!()

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
        post
        |> Ash.Changeset.for_update(:update, %{title: "Updated Title 2"})
        |> Ash.Changeset.load(related_posts: keyset_pagination_query)
        |> Ash.update!()

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

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "Title"})
        |> Ash.Changeset.manage_relationship(:related_posts, [related_post2, related_post1],
          type: :append_and_remove
        )
        |> Ash.create!()

      offset_pagination_query =
        Post
        |> Ash.Query.sort(title: :asc)
        |> Ash.Query.select([:title])
        |> Ash.Query.page(count: true, limit: 1)

      post =
        Ash.update!(post, %{title: "Updated Title 1"},
          load: [related_posts: offset_pagination_query]
        )

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
        Ash.update!(post, %{title: "Updated Title 2"},
          load: [related_posts: keyset_pagination_query]
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
      tag = Ash.create!(MultitenantTag, %{name: "tag"}, tenant: tenant)
      _ = Ash.create!(MultitenantTag, %{name: "existing"}, tenant: tenant)

      offset_pagination_query =
        MultitenantTag
        |> Ash.Query.sort(name: :asc)
        |> Ash.Query.select([:name])
        |> Ash.Query.page(count: true, limit: 1)

      tag =
        Ash.update!(tag, %{related_tags: ["existing"]},
          action: :add_related_tags,
          load: [related_tags: offset_pagination_query]
        )

      assert %Ash.Page.Keyset{
               results: [%MultitenantTag{name: "existing", __metadata__: %{keyset: keyset}}],
               limit: 1,
               count: 1,
               more?: false
             } = tag.related_tags

      keyset_pagination_query =
        MultitenantTag
        |> Ash.Query.sort(name: :asc)
        |> Ash.Query.select([:name])
        |> Ash.Query.page(count: true, limit: 1, after: keyset)

      tag =
        Ash.update!(tag, %{related_tags: ["new"]},
          action: :add_related_tags,
          load: [related_tags: keyset_pagination_query]
        )

      assert %Ash.Page.Keyset{
               results: [%MultitenantTag{name: "new"}],
               limit: 1,
               count: 2,
               more?: false,
               before: nil,
               after: ^keyset
             } = tag.related_tags
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

      assert_raise Ash.Error.Invalid, ~r/No such input `bio`/, fn ->
        author
        |> Ash.Changeset.for_update(:only_allow_name, %{bio: "bio"})
        |> Ash.update!()
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
        Ash.Changeset.fully_atomic_changeset(Author, :with_validation, %{name: "fred"},
          eager?: false
        )

      assert changeset.valid?
    end

    test "where condition is considered before checking validation atomicity" do
      changeset =
        Ash.Changeset.fully_atomic_changeset(
          Author,
          :with_partially_atomic_validation,
          %{match?: false},
          eager?: false
        )

      assert changeset.valid?

      # match validation can't be run atomically if the attribute is not changing
      assert {:not_atomic, _} =
               Ash.Changeset.fully_atomic_changeset(
                 Author,
                 :with_partially_atomic_validation,
                 %{match?: true},
                 eager?: false
               )
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

      assert_raise(Ash.Error.Forbidden, fn ->
        record
        |> Ash.Changeset.for_update(:update, %{name: "foo"})
        |> Ash.update!(authorize?: true, atomic_upgrade?: false)
      end)

      assert Ash.get!(Authorized, record.id, authorize?: false).name == "bar"

      stop_supervised!(Ash.Test.Authorizer)

      start_supervised({Ash.Test.Authorizer, check: :authorized, strict_check: :continue})

      record
      |> Ash.Changeset.for_update(:update, %{name: "foo"})
      |> Ash.update!(authorize?: true)
    end
  end
end
