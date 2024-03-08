defmodule Ash.Test.Changeset.ChangesetTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  require Ash.Query

  defmodule Category do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    identities do
      identity :unique_name, [:name], pre_check_with: Domain
    end

    actions do
      default_accept :*
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

      create :with_name_validation do
        validate present(:name), message: "this validates the name is present"
      end
    end

    attributes do
      uuid_primary_key :id

      attribute :name, :string do
        public?(true)
      end
    end

    relationships do
      many_to_many :posts, Ash.Test.Changeset.ChangesetTest.Post,
        public?: true,
        through: Ash.Test.Changeset.ChangesetTest.PostCategory,
        destination_attribute_on_join_resource: :post_id,
        source_attribute_on_join_resource: :category_id
    end
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
      default_accept :*
      defaults [:read, :create, :update, :destroy]
    end

    attributes do
      uuid_primary_key :id

      attribute :name, :string do
        public?(true)
      end
    end

    relationships do
      has_many :posts, Ash.Test.Changeset.ChangesetTest.Post,
        destination_attribute: :author_id,
        public?: true

      has_many :unique_posts, Ash.Test.Changeset.ChangesetTest.UniqueNamePerAuthor,
        destination_attribute: :author_id,
        public?: true

      has_many :composite_key_posts, Ash.Test.Changeset.ChangesetTest.CompositeKeyPost,
        destination_attribute: :author_id,
        public?: true
    end
  end

  defmodule PostCategory do
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
      attribute :priority, :integer, default: 0, public?: true
    end

    relationships do
      belongs_to :post, Ash.Test.Changeset.ChangesetTest.Post,
        public?: true,
        primary_key?: true,
        allow_nil?: false

      belongs_to :category, Ash.Test.Changeset.ChangesetTest.Category,
        public?: true,
        primary_key?: true,
        allow_nil?: false
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
      belongs_to :author, Author do
        public?(true)
      end

      many_to_many :categories, Ash.Test.Changeset.ChangesetTest.Category,
        public?: true,
        through: Ash.Test.Changeset.ChangesetTest.PostCategory,
        destination_attribute_on_join_resource: :category_id,
        source_attribute_on_join_resource: :post_id
    end
  end

  defmodule TenantPost do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, :create, :update, :destroy]
    end

    multitenancy do
      strategy :attribute
      attribute :tenant
    end

    attributes do
      uuid_primary_key :id

      attribute :title, :string do
        public?(true)
      end

      attribute :contents, :string do
        public?(true)
      end

      attribute :tenant, :string do
        public?(true)
      end
    end

    code_interface do
      define :create
    end

    relationships do
      belongs_to :author, Author do
        public?(true)
      end

      many_to_many :categories, Ash.Test.Changeset.ChangesetTest.Category,
        public?: true,
        through: Ash.Test.Changeset.ChangesetTest.PostCategory,
        destination_attribute_on_join_resource: :category_id,
        source_attribute_on_join_resource: :post_id
    end
  end

  defmodule CompositeKeyPost do
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
      attribute :serial, :integer, primary_key?: true, allow_nil?: false, public?: true
      attribute :title, :string, public?: true
      attribute :contents, :string, public?: true
    end

    relationships do
      belongs_to :author, Author do
        public?(true)
      end

      many_to_many :categories, Ash.Test.Changeset.ChangesetTest.Category,
        public?: true,
        through: Ash.Test.Changeset.ChangesetTest.PostCategory,
        destination_attribute_on_join_resource: :category_id,
        source_attribute_on_join_resource: :post_id
    end
  end

  defmodule UniqueNamePerAuthor do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, :create, :update, :destroy]
    end

    identities do
      identity :unique_name_per_author, [:title, :author_id], pre_check_with: Domain
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
      belongs_to :author, Author do
        public?(true)
      end

      many_to_many :categories, Ash.Test.Changeset.ChangesetTest.Category,
        public?: true,
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
      assert %Ash.Changeset{
               action_type: :create,
               attributes: %{},
               data: %Category{},
               errors: [],
               valid?: true
             } =
               Category
               |> Ash.Changeset.new()
    end

    test "it wraps a resource's record in an `update` changeset" do
      record =
        Category
        |> Ash.Changeset.for_create(:create, %{name: "foo"})
        |> Ash.create!()

      assert %Ash.Changeset{
               action_type: :update,
               attributes: %{},
               data: %Category{name: "foo"},
               errors: [],
               valid?: true
             } =
               record
               |> Ash.Changeset.new()
    end

    test "it raises an error for a non-resource record" do
      assert_raise ArgumentError,
                   ~r/`Ash.Test.Changeset.ChangesetTest.NonResource` is not a Spark DSL module/,
                   fn ->
                     %NonResource{name: "foo"}
                     |> Ash.Changeset.new()
                   end
    end
  end

  describe "with_hooks/2" do
    test "it applies a before_action function on a changeset" do
      capitalize_name = fn changeset = %Ash.Changeset{attributes: %{name: name}} ->
        %{
          changeset
          | attributes: Map.merge(changeset.attributes, %{name: String.capitalize(name)})
        }
      end

      changeset =
        Category
        |> Ash.Changeset.for_create(:create, %{name: "foo"})

      changeset = %{changeset | before_action: [capitalize_name]}

      category = changeset |> Ash.create!()

      assert %Category{name: "Foo"} = category
    end

    test "it applies a after_action function on a changeset" do
      capitalize_name = fn _changeset, result = %{name: _} ->
        {:ok, %{result | name: "modified"}}
      end

      changeset =
        Category
        |> Ash.Changeset.for_create(:create, %{name: "foo"})

      changeset = %{changeset | after_action: [capitalize_name]}

      category = changeset |> Ash.create!()
      assert %Category{name: "modified"} = category

      assert {:ok, %Category{name: "foo"}} = Ash.get(Category, category.id)
    end

    test "it applies an around_transaction function on a changeset" do
      change_name = fn changeset, callback ->
        %{attributes: %{name: name}} = changeset

        changeset = %{
          changeset
          | attributes: Map.merge(changeset.attributes, %{name: "#{name}_before"})
        }

        {:ok, result, changeset, notifications} = callback.(changeset)

        result = %{result | name: "#{result.name}_after"}

        {:ok, result, changeset, notifications}
      end

      changeset =
        Category
        |> Ash.Changeset.for_create(:create, %{name: "foo"})
        |> Ash.Changeset.around_transaction(change_name)

      category = changeset |> Ash.create!()

      assert %Category{name: "foo_before_after"} = category
    end
  end

  describe "get_attribute/2" do
    setup do
      category =
        Category
        |> Ash.Changeset.for_create(:create, %{name: "foo"})
        |> Ash.create!()

      {:ok, %{category: category}}
    end

    test "it get an attribute of the changeset", %{category: category} do
      changeset =
        category
        |> Ash.Changeset.for_update(:update, %{name: "bar"})

      assert "bar" == Ash.Changeset.get_attribute(changeset, :name)
    end

    test "it falls back to the original value of the attribute", %{category: category} do
      changeset =
        category
        |> Ash.Changeset.new()

      assert "foo" == Ash.Changeset.get_attribute(changeset, :name)
    end

    test "it returns nil if the attribute not found" do
      changeset =
        Category
        |> Ash.Changeset.new()

      assert %{} == changeset.attributes

      assert is_nil(Ash.Changeset.get_attribute(changeset, :not_there))
    end
  end

  describe "fetch_change/2" do
    test "it get a changed attribute of the changeset" do
      category =
        Category
        |> Ash.Changeset.for_create(:create, %{name: "foo"})
        |> Ash.create!()

      changeset =
        category
        |> Ash.Changeset.for_update(:update, %{name: "bar"})

      assert {:ok, "bar"} == Ash.Changeset.fetch_change(changeset, :name)
    end

    test "it returns :error if the attribute not found" do
      changeset =
        Category
        |> Ash.Changeset.new()

      assert %{} == changeset.attributes

      assert :error == Ash.Changeset.fetch_change(changeset, :none)
    end
  end

  describe "set_context/2" do
    test "it sets context to a given map" do
      changeset =
        Category
        |> Ash.Changeset.new()
        |> Ash.Changeset.set_context(%{key: "value"})

      assert "value" == changeset.context.key
    end
  end

  describe "put_context/2" do
    test "it puts a value in a context key" do
      changeset =
        Category
        |> Ash.Changeset.new()
        |> Ash.Changeset.put_context(:key, "value")

      assert "value" == changeset.context.key
    end
  end

  describe "manage_relationship/3" do
    test "it works for belongs_to relationships" do
      author = %{name: "title"}

      post =
        Post
        |> Ash.Changeset.new()
        |> Ash.Changeset.manage_relationship(:author, author, on_no_match: :create)
        |> Ash.create!()

      assert [%{name: "title"}] = Ash.read!(Author)

      assert %{name: "title"} = Ash.load!(post, :author).author
    end

    test "it removes belongs_to entities on replace" do
      author = %{name: "title"}

      post =
        Post
        |> Ash.Changeset.new()
        |> Ash.Changeset.manage_relationship(:author, author, on_no_match: :create)
        |> Ash.create!()

      new_author = %{name: "title2"}

      post =
        post
        |> Ash.Changeset.new()
        |> Ash.Changeset.manage_relationship(:author, new_author,
          on_no_match: :create,
          on_missing: :destroy
        )
        |> Ash.update!()

      assert [%{name: "title2"}] = Ash.read!(Author)

      assert %{name: "title2"} = Ash.load!(post, :author).author
    end

    test "upsert with many_to_many relationships creates and relates records, and returns the created/related records" do
      Category
      |> Ash.Changeset.for_create(:create, %{name: "foo"})
      |> Ash.create!()

      post =
        Post
        |> Ash.Changeset.new()
        |> Ash.Changeset.manage_relationship(:categories, [%{name: "foo"}, %{name: "bar"}],
          on_lookup: :relate,
          on_no_match: :create
        )
        |> Ash.create!()

      assert [%{name: "bar"}, %{name: "foo"}] = Enum.sort_by(post.categories, & &1.name)
    end

    test "value_is_key option determines what single values are keyed as" do
      changeset =
        Post
        |> Ash.Changeset.new()
        |> Ash.Changeset.manage_relationship(:categories, ["foo", "bar"],
          on_lookup: :relate,
          on_no_match: :create
        )

      inputs = changeset.relationships.categories |> Enum.at(0) |> elem(0)

      assert inputs == [%{id: "foo"}, %{id: "bar"}]

      changeset =
        Post
        |> Ash.Changeset.new()
        |> Ash.Changeset.manage_relationship(:categories, ["foo", "bar"],
          value_is_key: :name,
          on_lookup: :relate,
          on_no_match: :create
        )

      inputs = changeset.relationships.categories |> Enum.at(0) |> elem(0)

      assert inputs == [%{name: "foo"}, %{name: "bar"}]
    end

    test "upsert with many_to_many relationships can eager validate" do
      Category
      |> Ash.Changeset.for_create(:create, %{name: "foo"})
      |> Ash.create!()

      assert %{valid?: false, errors: [%Ash.Error.Query.NotFound{}]} =
               Post
               |> Ash.Changeset.new()
               |> Ash.Changeset.manage_relationship(:categories, [%{name: "foo"}, %{name: "bar"}],
                 on_lookup: :relate,
                 eager_validate_with: Ash,
                 use_identities: [:unique_name]
               )

      assert %{valid?: true} =
               Post
               |> Ash.Changeset.new()
               |> Ash.Changeset.manage_relationship(:categories, [%{name: "foo"}],
                 on_lookup: :relate,
                 eager_validate_with: Ash,
                 use_identities: [:unique_name]
               )
    end

    test "it creates related entities" do
      post1 = %{title: "title"}
      post2 = %{title: "title"}

      author =
        Author
        |> Ash.Changeset.new()
        |> Ash.Changeset.manage_relationship(:posts, [post1, post2], on_no_match: :create)
        |> Ash.create!()

      assert [%{title: "title"}, %{title: "title"}] = Ash.read!(Post)

      assert %{posts: [%{title: "title"}, %{title: "title"}]} = Ash.load!(author, :posts)
    end

    test "it ignores creates if configured" do
      post1 = %{title: "title"}
      post2 = %{title: "title"}

      author =
        Author
        |> Ash.Changeset.new()
        |> Ash.Changeset.manage_relationship(:posts, [post1, post2], on_no_match: :ignore)
        |> Ash.create!()

      assert [] = Ash.read!(Post)

      assert %{posts: []} = Ash.load!(author, :posts)
    end

    test "it errors on creates if configured" do
      post1 = %{title: "title"}
      post2 = %{title: "title"}

      assert_raise Ash.Error.Invalid,
                   ~r/Invalid value provided for posts: changes would create a new related record/,
                   fn ->
                     Author
                     |> Ash.Changeset.new()
                     |> Ash.Changeset.manage_relationship(:posts, [post1, post2],
                       on_no_match: :error
                     )
                     |> Ash.create!()
                   end
    end

    test "it destroys related records if not present" do
      post1 = %{title: "title"}
      post2 = %{title: "title"}

      author =
        Author
        |> Ash.Changeset.new()
        |> Ash.Changeset.manage_relationship(:posts, [post1, post2], on_no_match: :create)
        |> Ash.create!()

      assert [%{title: "title"}, %{title: "title"}] = Ash.read!(Post)

      author
      |> Ash.Changeset.new()
      |> Ash.Changeset.manage_relationship(:posts, [], on_missing: :destroy)
      |> Ash.update!()

      assert [] = Ash.read!(Post)
    end

    test "it unrelates records if specified" do
      post1 = %{title: "title"}
      post2 = %{title: "title1"}

      author =
        Author
        |> Ash.Changeset.new()
        |> Ash.Changeset.manage_relationship(:unique_posts, [post1, post2],
          on_no_match: :create,
          use_identities: [:unique_name_per_author]
        )
        |> Ash.create!()

      assert [%{title: "title"}, %{title: "title1"}] =
               Enum.sort_by(Ash.read!(UniqueNamePerAuthor), & &1.title)

      author =
        author
        |> Ash.Changeset.new()
        |> Ash.Changeset.manage_relationship(:unique_posts, [%{title: "title"}],
          on_missing: :unrelate,
          use_identities: [:unique_name_per_author]
        )
        |> Ash.update!()

      assert [%{title: "title"}, %{title: "title1"}] =
               Enum.sort_by(Ash.read!(UniqueNamePerAuthor), & &1.title)

      assert %{unique_posts: [%{title: "title"}]} = Ash.load!(author, :unique_posts)
    end

    test "it properly assumes the destination field of the relationship matches records when not provided" do
      post1 = %{title: "title"}
      post2 = %{title: "title"}

      author =
        Author
        |> Ash.Changeset.new()
        |> Ash.Changeset.manage_relationship(:posts, [post1, post2], on_no_match: :create)
        |> Ash.create!()

      assert [%{title: "title"}, %{title: "title"}] = Ash.read!(Post)

      author =
        author
        |> Ash.Changeset.new()
        |> Ash.Changeset.manage_relationship(:posts, [], on_missing: :unrelate)
        |> Ash.update!()

      assert [%{title: "title"}, %{title: "title"}] = Ash.read!(Post)

      assert %{posts: []} = Ash.load!(author, :posts)
    end

    test "it updates records" do
      post1 = %{title: "title"}
      post2 = %{title: "title"}

      author =
        Author
        |> Ash.Changeset.new()
        |> Ash.Changeset.manage_relationship(:posts, [post1, post2], on_no_match: :create)
        |> Ash.create!()

      assert posts = [%{title: "title"}, %{title: "title"}] = Ash.read!(Post)

      post_ids = Enum.map(posts, &Map.get(&1, :id))
      input = Enum.map(post_ids, &%{"id" => &1, title: "new_title"})

      author
      |> Ash.Changeset.new()
      |> Ash.Changeset.manage_relationship(:posts, input, on_match: :update)
      |> Ash.update!()

      assert [%{title: "new_title"}, %{title: "new_title"}] = Ash.read!(Post)
    end

    test "it updates only join records in many_to_many relationships with on_match: :update_join" do
      post =
        Post
        |> Ash.Changeset.new()
        |> Ash.Changeset.manage_relationship(
          :categories,
          [%{name: "foo", priority: 0}, %{name: "bar", priority: 1}],
          on_no_match: :create,
          join_keys: [:priority]
        )
        |> Ash.create!()

      assert [%{id: foo_id, name: "foo"}, %{id: bar_id, name: "bar"}] =
               Ash.read!(Category) |> Enum.sort_by(& &1.name, :desc)

      assert [%{category_id: ^foo_id, priority: 0}, %{category_id: ^bar_id, priority: 1}] =
               Ash.read!(PostCategory) |> Enum.sort_by(& &1.priority)

      post
      |> Ash.Changeset.new()
      |> Ash.Changeset.manage_relationship(
        :categories,
        [%{name: "foo", priority: 2}],
        on_match: :update_join,
        use_identities: [:unique_name],
        join_keys: [:priority]
      )
      |> Ash.update!()

      assert [%{id: ^foo_id, name: "foo"}, %{id: ^bar_id, name: "bar"}] =
               Ash.read!(Category) |> Enum.sort_by(& &1.name, :desc)

      assert [%{category_id: ^bar_id, priority: 1}, %{category_id: ^foo_id, priority: 2}] =
               Ash.read!(PostCategory) |> Enum.sort_by(& &1.priority)
    end
  end

  describe "manage_relationship/3 type: :append_and_remove" do
    test "it replaces entities to a resource's relationship" do
      post1 = Post |> Ash.Changeset.for_create(:create, %{title: "title1"}) |> Ash.create!()
      post2 = Post |> Ash.Changeset.for_create(:create, %{title: "title2"}) |> Ash.create!()

      author =
        Author
        |> Ash.Changeset.new()
        |> Ash.Changeset.manage_relationship(:posts, [post1], type: :append_and_remove)
        |> Ash.create!()

      [author] =
        Author
        |> Ash.Query.load(posts: [:author])
        |> Ash.Query.filter(id == ^author.id)
        |> Ash.read!()

      assert [author_post] = author.posts

      assert author_post.id == post1.id

      author =
        author
        |> Ash.Changeset.new()
        |> Ash.Changeset.manage_relationship(:posts, [post2], type: :append_and_remove)
        |> Ash.update!()

      [author] =
        Author
        |> Ash.Query.load(posts: [:author])
        |> Ash.Query.filter(id == ^author.id)
        |> Ash.read!()

      assert [author_post] = author.posts

      assert author_post.id == post2.id
    end

    test "it accepts a map %{att1: value1, att2: value2} representing primary key as a second param" do
      post1 =
        CompositeKeyPost
        |> Ash.Changeset.for_create(:create, %{serial: 1})
        |> Ash.create!()

      author =
        Author
        |> Ash.Changeset.new()
        |> Ash.Changeset.manage_relationship(
          :composite_key_posts,
          [%{id: post1.id, serial: post1.serial}],
          type: :append_and_remove
        )
        |> Ash.create!()

      [fetched_post] =
        CompositeKeyPost
        |> Ash.Query.load(author: :composite_key_posts)
        |> Ash.Query.filter(id == ^post1.id and serial == ^post1.serial)
        |> Ash.read!()

      assert Ash.reload!(author) == Ash.reload!(fetched_post.author)
    end

    test "it accepts a list of maps representing primary_keys as a second param" do
      post1 =
        CompositeKeyPost
        |> Ash.Changeset.for_create(:create, %{serial: 1})
        |> Ash.create!()

      post2 =
        CompositeKeyPost
        |> Ash.Changeset.for_create(:create, %{serial: 2})
        |> Ash.create!()

      author =
        Author
        |> Ash.Changeset.new()
        |> Ash.Changeset.manage_relationship(
          :composite_key_posts,
          [
            %{id: post1.id, serial: post1.serial},
            %{id: post2.id, serial: post2.serial}
          ],
          type: :append_and_remove
        )
        |> Ash.create!()

      [fetched_post] =
        CompositeKeyPost
        |> Ash.Query.load(author: :composite_key_posts)
        |> Ash.Query.filter(id == ^post1.id and serial == ^post1.serial)
        |> Ash.read!()

      assert Ash.reload!(author) == Ash.reload!(fetched_post.author)
    end

    test "it accepts mix of entities and maps representing primary_keys as a second param" do
      post1 =
        CompositeKeyPost
        |> Ash.Changeset.for_create(:create, %{serial: 1})
        |> Ash.create!()

      post2 =
        CompositeKeyPost
        |> Ash.Changeset.for_create(:create, %{serial: 2})
        |> Ash.create!()

      author =
        Author
        |> Ash.Changeset.new()
        |> Ash.Changeset.manage_relationship(
          :composite_key_posts,
          [
            %{id: post1.id, serial: post1.serial},
            post2
          ],
          type: :append_and_remove
        )
        |> Ash.create!()

      [fetched_author] =
        Author
        |> Ash.Query.load(:composite_key_posts)
        |> Ash.Query.filter(id == ^author.id)
        |> Ash.read!()

      assert Enum.sort(Enum.map(fetched_author.composite_key_posts, & &1.id)) ==
               Enum.sort([post1.id, post2.id])
    end

    test "it returns error if one of relationship entities is invalid" do
      post1 =
        CompositeKeyPost
        |> Ash.Changeset.for_create(:create, %{serial: 1})
        |> Ash.create!()

      post2 =
        CompositeKeyPost
        |> Ash.Changeset.for_create(:create, %{serial: 2})
        |> Ash.create!()

      invalid_post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "a title"})
        |> Ash.create!()

      changeset =
        Author
        |> Ash.Changeset.new()
        |> Ash.Changeset.manage_relationship(
          :composite_key_posts,
          [
            %{id: post1.id, serial: post1.serial},
            post2,
            invalid_post
          ],
          type: :append_and_remove
        )

      assert [%Ash.Error.Changes.InvalidRelationship{} = relation_error] = changeset.errors
      assert relation_error.message =~ "cannot provide structs that don't match the destination"
    end

    test "it returns error if relationship does not exists" do
      post1 = Post |> Ash.Changeset.for_create(:create, %{title: "foo"}) |> Ash.create!()

      changeset =
        Author
        |> Ash.Changeset.new()
        |> Ash.Changeset.manage_relationship(:na, post1, type: :append_and_remove)

      assert %{} == changeset.relationships
      assert [%Ash.Error.Changes.NoSuchRelationship{}] = changeset.errors
    end
  end

  describe "changing_attribute?/2" do
    test "it returns true if the attribute is being changed by the current changeset" do
      changeset = Post |> Ash.Changeset.for_create(:create, %{title: "title1"})
      assert Ash.Changeset.changing_attribute?(changeset, :title)
    end

    test "it returns false if the attribute is NOT being changed by the current changeset" do
      changeset = Post |> Ash.Changeset.for_create(:create, %{title: "title1"})
      refute Ash.Changeset.changing_attribute?(changeset, :contents)
    end
  end

  describe "changing_relationship?/2" do
    test "it returns true if the attribute is being changed by the current changeset" do
      post = Post |> Ash.Changeset.for_create(:create, %{title: "title2"}) |> Ash.create!()

      changeset =
        Author
        |> Ash.Changeset.new()
        |> Ash.Changeset.manage_relationship(:posts, [post], type: :append_and_remove)

      assert Ash.Changeset.changing_relationship?(changeset, :posts)
    end

    test "it returns false if the attribute is NOT being changed by the current changeset" do
      changeset = Post |> Ash.Changeset.for_create(:create, %{title: "title1"})
      refute Ash.Changeset.changing_relationship?(changeset, :posts)
    end
  end

  describe "change_new_attribute/3" do
    test "it changes attribute if it's not currently being changed" do
      changeset =
        Post
        |> Ash.Changeset.new()
        |> Ash.Changeset.change_new_attribute(:contents, "some content")
        |> Ash.Changeset.for_create(:create, %{title: "title1"})

      assert %Ash.Changeset{attributes: %{title: "title1", contents: "some content"}} = changeset
    end

    test "it keeps the current value of attribute if it's currently being changed" do
      changeset =
        Post
        |> Ash.Changeset.new()
        |> Ash.Changeset.change_new_attribute(:title, "another title")
        |> Ash.Changeset.for_create(:create, %{title: "title1"})

      assert %Ash.Changeset{
               attributes: %{
                 title: "title1"
               }
             } = changeset
    end
  end

  describe "arguments" do
    test "arguments can be used in valid changes" do
      Category
      |> Ash.Changeset.new()
      |> Ash.Changeset.set_argument(:confirm_name, "foo")
      |> Ash.Changeset.for_create(:create_with_confirmation, %{"name" => "foo"})
      |> Ash.create!()
    end

    test "arguments can be provided as strings" do
      Category
      |> Ash.Changeset.new()
      |> Ash.Changeset.set_argument("confirm_name", "foo")
      |> Ash.Changeset.for_create(:create_with_confirmation, %{"name" => "foo"})
      |> Ash.create!()
    end

    test "arguments can be used in invalid changes" do
      assert_raise Ash.Error.Invalid, ~r/confirmation did not match value/, fn ->
        Category
        |> Ash.Changeset.new()
        |> Ash.Changeset.set_argument(:confirm_name, "bar")
        |> Ash.Changeset.for_create(:create_with_confirmation, %{"name" => "foo"})
        |> Ash.create!()
      end
    end

    test "required arguments can't be nil" do
      assert_raise Ash.Error.Invalid, ~r/argument confirm_name is required/, fn ->
        Category
        |> Ash.Changeset.for_create(:create_with_confirmation, %{"name" => "foo"})
        |> Ash.create!()
      end
    end

    test "optional arguments should use the default" do
      changeset =
        Category
        |> Ash.Changeset.for_create(:create_with_confirmation)

      assert Ash.Changeset.get_argument(changeset, :true_optional_argument) == true
      assert Ash.Changeset.get_argument(changeset, :false_optional_argument) == false
    end
  end

  describe "for_<action>" do
    test "arguments are validated" do
      assert [
               %Ash.Error.Changes.InvalidAttribute{
                 class: :invalid,
                 field: :confirm_name,
                 message: "confirmation did not match value",
                 path: []
               }
             ] =
               Ash.Changeset.for_create(Category, :create_with_confirmation, %{
                 "name" => "foo",
                 "confirm_name" => "bar"
               }).errors
    end

    test "for_action works the same as calling for_<action>" do
      changeset_1 =
        Category
        |> Ash.Changeset.for_create(:create)

      changeset_2 =
        Category
        |> Ash.Changeset.for_action(:create)

      assert changeset_1 == changeset_2
    end

    test "a message can be specified as part of a `present` validation" do
      assert [
               %Ash.Error.Changes.InvalidAttribute{
                 class: :invalid,
                 field: :name,
                 message: "this validates the name is present",
                 path: []
               }
             ] =
               Ash.Changeset.for_create(Category, :with_name_validation, %{"name" => ""}).errors
    end
  end
end
