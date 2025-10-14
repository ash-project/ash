# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.GeneratorTest do
  @moduledoc false
  use ExUnit.Case, async: true
  use ExUnitProperties
  import Ash.Seed
  require Ash.Query
  alias Ash.Test.Domain, as: Domain

  defmodule Notifier do
    use Ash.Notifier

    def notify(notification) do
      send(Application.get_env(__MODULE__, :notifier_test_pid), {:notification, notification})
    end
  end

  defmodule Embedded do
    use Ash.Resource, data_layer: :embedded

    actions do
      defaults [:read, :destroy, create: :*, update: :*]
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string
    end
  end

  defmodule Author do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]

      update :new_post do
        require_atomic? false
        argument :posts, {:array, :map}, allow_nil?: false
        change manage_relationship(:posts, type: :create)
      end

      update :new_post_private do
        require_atomic? false
        argument :posts, {:array, :map}, allow_nil?: false, public?: false
        change manage_relationship(:posts, type: :create)
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string, default: "Fred", public?: true

      attribute :metadata, :map do
        public?(true)
        allow_nil? true
      end

      attribute :meta, :map do
        public?(true)
        allow_nil? false
      end
    end

    relationships do
      has_many :posts, Ash.Test.GeneratorTest.Post,
        destination_attribute: :author_id,
        public?: true

      has_one :latest_post, Ash.Test.GeneratorTest.Post,
        destination_attribute: :author_id,
        sort: [inserted_at: :desc],
        public?: true
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets, notifiers: [Notifier]

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]

      read :read_with_args do
        argument :title_filter, :string
        argument :category_filter, :string
      end

      destroy :destroy_with_reason do
        argument :reason, :string
        argument :confirmed, :boolean, default: false
      end
    end

    attributes do
      uuid_primary_key :id

      attribute :title, :string do
        public?(true)
      end

      attribute :title_again, :string do
        public?(true)
      end

      attribute :contents, :string do
        public?(true)
      end

      attribute :category, :string do
        public?(true)
      end

      attribute :embedded, Embedded do
        public? true
        allow_nil? false
      end

      timestamps()
    end

    relationships do
      belongs_to :author, Author, public?: true

      has_many :ratings, Ash.Test.GeneratorTest.Rating, public?: true

      many_to_many :categories, Ash.Test.GeneratorTest.Category,
        through: Ash.Test.GeneratorTest.PostCategory,
        destination_attribute_on_join_resource: :category_id,
        source_attribute_on_join_resource: :post_id,
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
      defaults [:read, :destroy, create: :*, update: :*]
    end

    relationships do
      belongs_to :post, Post, primary_key?: true, allow_nil?: false, public?: true

      belongs_to :category, Ash.Test.GeneratorTest.Category,
        primary_key?: true,
        allow_nil?: false,
        public?: true
    end
  end

  defmodule Category do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    identities do
      identity :unique_name, [:name], pre_check_with: Domain
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    attributes do
      uuid_primary_key :id

      attribute :name, :string do
        public?(true)
      end
    end

    relationships do
      many_to_many :posts, Post,
        public?: true,
        through: PostCategory,
        destination_attribute_on_join_resource: :post_id,
        source_attribute_on_join_resource: :category_id
    end
  end

  defmodule Rating do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id

      attribute :rating, :integer do
        public?(true)
      end
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    relationships do
      belongs_to :post, Post do
        public?(true)
        domain(Ash.Test.GeneratorTest.Category)
      end
    end
  end

  defmodule Generator do
    use Ash.Generator

    def author(opts \\ []) do
      changeset_generator(Author, :create, overrides: opts)
    end

    def seed_post(opts \\ []) do
      {ratings, opts} =
        Keyword.pop(opts, :ratings)

      seed_generator(
        %Post{
          title: sequence(:title, &"Post #{&1}")
        },
        after_action: fn record ->
          if ratings do
            ratings
            |> Enum.map(&Map.put(&1, :post_id, record.id))
            |> Ash.bulk_create(Rating, :create)

            record
            |> Ash.load!(:ratings)
          else
            record
          end
        end,
        overrides: opts
      )
    end

    def post(opts \\ []) do
      changeset_generator(Post, :create,
        defaults: [title: sequence(:title, &"Post #{&1}")],
        overrides: opts
      )
    end

    def post_with_double_name(opts \\ []) do
      changeset_generator(Post, :create,
        uses: %{
          name: sequence(:title, &"Post #{&1}")
        },
        defaults: fn uses ->
          [title: uses.name, title_again: uses.name]
        end,
        overrides: opts
      )
    end

    def post_for(author, opts \\ []) do
      changeset_generator(author, :new_post,
        uses: [
          post_input:
            action_input(
              Post,
              :create,
              name: sequence(:title, &"Post #{&1}")
            )
        ],
        defaults: fn inputs ->
          [
            posts: [inputs.post_input]
          ]
        end,
        overrides: opts
      )
    end

    def post_for_private(author, opts \\ []) do
      changeset_generator(author, :new_post_private,
        uses: [
          post_input:
            action_input(
              Post,
              :create,
              name: sequence(:title, &"Post #{&1}")
            )
        ],
        private_arguments: fn inputs ->
          %{
            posts: [inputs.post_input]
          }
        end,
        overrides: opts
      )
    end

    def seed_post_with_double_name(opts \\ []) do
      seed_generator(
        fn uses ->
          %Post{
            title: uses.name,
            title_again: uses.name
          }
        end,
        uses: %{
          name: sequence(:title, &"Post #{&1}")
        },
        overrides: opts
      )
    end

    def embedded(opts \\ []) do
      changeset_generator(Embedded, :create,
        defaults: [
          title: sequence(:title, &"Post #{&1}")
        ],
        overrides: opts
      )
    end
  end

  setup do
    Application.put_env(Notifier, :notifier_test_pid, self())
  end

  describe "generator" do
    test "can generate one" do
      import Generator
      assert %Post{title: "Post 0"} = generate(post())
      assert %Post{title: "Post 1"} = generate(seed_post())
    end

    test "can generate many" do
      import Generator
      assert [%Post{title: "Post 0"}, %Post{title: "Post 1"}] = generate_many(post(), 2)
      assert [%Post{title: "Post 2"}, %Post{title: "Post 3"}] = generate_many(seed_post(), 2)
    end

    test "after_action works with generate" do
      import Generator

      assert %{title: "Post 0", ratings: [%{rating: 5}]} =
               generate(seed_post(ratings: [%{rating: 5}]))
    end

    test "after_action works with generate_many" do
      import Generator

      assert [
               %{title: "Post 0", ratings: [%{rating: 5}]},
               %{title: "Post 1", ratings: [%{rating: 5}]}
             ] =
               generate_many(seed_post(ratings: [%{rating: 5}]), 2)
    end

    test "can share generators with `uses`" do
      import Generator

      assert [
               %Post{title: "Post 0", title_again: "Post 0"},
               %Post{title: "Post 1", title_again: "Post 1"}
             ] =
               generate_many(post_with_double_name(), 2)

      assert [
               %Post{title: "Post 2", title_again: "Post 2"},
               %Post{title: "Post 3", title_again: "Post 3"}
             ] =
               generate_many(seed_post_with_double_name(), 2)
    end

    test "can generate managed" do
      import Generator
      author = generate(author())

      author_with_posts = generate(post_for(author))
      assert [%Post{}] = author_with_posts.posts
    end

    test "can generate private arguments" do
      import Generator
      author = generate(author())

      author_with_posts = generate(post_for_private(author))
      assert [%Post{}] = author_with_posts.posts
    end

    test "errors are raised when generating invalid single items" do
      import Generator

      assert_raise Ash.Error.Invalid, ~r/Invalid value provided for title/, fn ->
        generate(post(title: %{a: :b}))
      end

      assert_raise Ash.Error.Invalid, ~r/Invalid value provided for title/, fn ->
        generate(seed_post(title: %{a: :b}))
      end
    end

    test "errors are raised when generating invalid many items" do
      import Generator

      assert_raise Ash.Error.Invalid, ~r/Invalid value provided for title/, fn ->
        generate_many(post(title: %{a: :b}), 2)
      end

      assert_raise Ash.Error.Invalid, ~r/Invalid value provided for title/, fn ->
        generate_many(post(title: %{a: :b}), 2)
      end
    end

    test "notifications are emitted on generate_many" do
      import Generator
      assert [%Post{title: "Post 0"}, %Post{title: "Post 1"}] = generate_many(post(), 2)
      assert_receive {:notification, %Ash.Notifier.Notification{}}
      assert_receive {:notification, %Ash.Notifier.Notification{}}
    end

    test "can generate embedded resource" do
      import Generator

      assert %Embedded{} = generate(embedded())
      Ash.Generator.action_input(Embedded, :create) |> Enum.take(1)
    end
  end

  describe "once" do
    test "generates the same value given the same name" do
      assert Enum.at(Ash.Generator.once(:name, fn -> 1 end), 0) ==
               Enum.at(Ash.Generator.once(:name, fn -> 2 end), 0)
    end

    test "generates the same value given the same name, but only in the same process" do
      assert Task.await(Task.async(fn -> Enum.at(Ash.Generator.once(:name, fn -> 1 end), 0) end)) !=
               Task.await(
                 Task.async(fn -> Enum.at(Ash.Generator.once(:name, fn -> 2 end), 0) end)
               )
    end
  end

  describe "action_input" do
    test "action input works with create actions" do
      check all(input <- Ash.Generator.action_input(Post, :create)) do
        Post
        |> Ash.Changeset.for_create(:create, input)
        |> Ash.create!()
      end
    end

    test "overrides work with create actions" do
      check all(
              input <-
                Ash.Generator.action_input(Post, :create, %{title: "override_test"})
            ) do
        post =
          Post
          |> Ash.Changeset.for_create(:create, input)
          |> Ash.create!()

        assert post.title == "override_test"
      end
    end

    test "action input works with read actions" do
      check all(input <- Ash.Generator.action_input(Post, :read)) do
        Post
        |> Ash.Query.for_read(:read, input)
        |> Ash.read!()
      end
    end

    test "action input works with read actions that have arguments" do
      check all(input <- Ash.Generator.action_input(Post, :read_with_args)) do
        Post
        |> Ash.Query.for_read(:read_with_args, input)
        |> Ash.read!()
      end
    end

    test "overrides work with read actions" do
      check all(
              input <-
                Ash.Generator.action_input(Post, :read_with_args, %{title_filter: "test_filter"})
            ) do
        assert input.title_filter == "test_filter"

        Post
        |> Ash.Query.for_read(:read_with_args, input)
        |> Ash.read!()
      end
    end

    test "action input works with update actions" do
      import Generator

      post = generate(post())

      check all(input <- Ash.Generator.action_input(post, :update)) do
        post
        |> Ash.Changeset.for_update(:update, input)
        |> Ash.update!()
      end
    end

    test "overrides work with update actions" do
      import Generator

      post = generate(post())

      check all(
              input <-
                Ash.Generator.action_input(post, :update, %{title: "updated_title"})
            ) do
        updated_post =
          post
          |> Ash.Changeset.for_update(:update, input)
          |> Ash.update!()

        assert updated_post.title == "updated_title"
      end
    end

    test "action input works with destroy actions" do
      import Generator

      check all(input <- Ash.Generator.action_input(Post, :destroy)) do
        post = generate(post())

        post
        |> Ash.Changeset.for_destroy(:destroy, input)
        |> Ash.destroy!()
      end
    end

    test "action input works with destroy actions that have arguments" do
      import Generator

      check all(input <- Ash.Generator.action_input(Post, :destroy_with_reason)) do
        post = generate(post())

        post
        |> Ash.Changeset.for_destroy(:destroy_with_reason, input)
        |> Ash.destroy!()
      end
    end

    test "overrides work with destroy actions" do
      import Generator

      check all(
              input <-
                Ash.Generator.action_input(Post, :destroy_with_reason, %{
                  reason: "test_deletion",
                  confirmed: true
                })
            ) do
        post = generate(post())

        assert input.reason == "test_deletion"
        assert input.confirmed == true

        post
        |> Ash.Changeset.for_destroy(:destroy_with_reason, input)
        |> Ash.destroy!()
      end
    end
  end

  test "string generator honors trim?: true" do
    check all(string <- Ash.Type.String.generator(min_length: 5, trim?: true)) do
      assert String.length(String.trim(string)) >= 5
    end
  end

  describe "changeset" do
    test "a directly usable changeset can be created" do
      Post
      |> Ash.Generator.changeset(:create)
      |> Ash.create!()
    end

    test "many changesets can be generated" do
      posts =
        Post
        |> Ash.Generator.many_changesets(:create, 5)
        |> Enum.map(&Ash.create!/1)

      assert Enum.count(posts) == 5
    end
  end

  @meta_generator %{
    meta: %{},
    metadata: %{}
  }

  describe "seed_input" do
    test "it returns attributes generated" do
      Author
      |> Ash.Generator.seed_input(@meta_generator)
      |> Enum.take(10)
      |> Enum.each(fn input ->
        seed!(Author, input)
      end)
    end

    defmodule Factory do
      def post(params \\ %{}) do
        defaults = %{
          name: StreamData.repeatedly(&Ash.UUID.generate/0)
        }

        Ash.Generator.seed_input(Post, Map.merge(defaults, params))
      end

      def author(params \\ %{}) do
        defaults = %{
          name: StreamData.repeatedly(&Ash.UUID.generate/0),
          posts: StreamData.list_of(post(), min_length: 1, max_length: 5),
          meta: %{},
          metadata: %{}
        }

        Ash.Generator.seed_input(Author, Map.merge(defaults, params))
      end
    end

    test "individual constructors can be supplied" do
      check all(input <- Factory.author()) do
        post_count = Enum.count(seed!(Author, input).posts)
        assert post_count >= 1
        assert post_count <= 5
      end
    end

    test "it can be used in property testing" do
      check all(input <- Ash.Generator.seed_input(Author, @meta_generator)) do
        seed!(Author, input)
      end
    end

    test "many seeds can be generated with seed_many!" do
      assert Enum.count(Ash.Generator.seed_many!(Author, 5, @meta_generator)) == 5
    end
  end

  describe "seed_generator" do
    test "it correctly seeds a record" do
      assert %Author{} =
               Ash.Generator.seed_generator(%Author{}, overrides: @meta_generator)
               |> Ash.Generator.generate()
    end
  end

  describe "changeset_generator" do
    test "it correctly seeds a record" do
      assert %Author{} =
               Ash.Generator.changeset_generator(Author, :create, overrides: @meta_generator)
               |> Ash.Generator.generate()
    end

    test "it raises an error for an invalid action name" do
      assert_raise ArgumentError,
                   "Invalid action Ash.Test.GeneratorTest.Author.jeremy_bearimy",
                   fn ->
                     Ash.Generator.changeset_generator(Author, :jeremy_bearimy)
                   end
    end
  end

  describe "seed" do
    test "it seeds correctly a resource" do
      assert %Author{} = Ash.Generator.seed!(Author, @meta_generator)
    end

    test "it works with the value :__keep_nil__" do
      assert %Author{metadata: nil} =
               Ash.Generator.seed!(Author, %{meta: %{}, metadata: keep_nil()})
    end

    test "it works with the value :__skip__" do
      assert %Author{metadata: nil} =
               Ash.Generator.seed!(Author, %{meta: %{}, metadata: skip()})
    end
  end

  describe "built in generators" do
    for type <- Enum.uniq(Ash.Type.builtin_types()),
        type not in [
          Ash.Type.Struct,
          Ash.Type.Keyword,
          Ash.Type.Map,
          Ash.Type.Tuple,
          Ash.Type.Union
        ] do
      for type <- [{:array, type}, type] do
        constraints =
          case type do
            {:array, type} ->
              [items: Spark.Options.validate!([], Ash.Type.constraints(type))]

            type ->
              Spark.Options.validate!([], Ash.Type.constraints(type))
          end

        test "#{inspect(type)} type can be generated" do
          try do
            check all(input <- Ash.Type.generator(unquote(type), unquote(constraints))) do
              {:ok, _} = Ash.Type.cast_input(unquote(type), input, unquote(constraints))
            end
          rescue
            e in RuntimeError ->
              case e do
                %RuntimeError{message: "generator/1 unimplemented for" <> _} ->
                  :ok

                other ->
                  reraise other, __STACKTRACE__
              end
          end
        end
      end
    end
  end

  describe "query" do
    test "a directly usable query can be created" do
      Post
      |> Ash.Generator.query(:read_with_args)
      |> Ash.read!()
    end
  end
end
