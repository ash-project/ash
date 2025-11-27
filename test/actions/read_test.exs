# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Actions.ReadTest do
  @moduledoc false
  use ExUnit.Case, async: true

  import Ash.Test

  require Ash.Query

  alias Ash.Test.Domain, as: Domain
  require Ash.Flags

  defmodule PostPreparation do
    @moduledoc false
    use Ash.Resource.Preparation

    def prepare(query, _, _) do
      Ash.Query.after_action(query, fn _query, posts ->
        {:ok, Enum.map(posts, &Ash.Resource.set_metadata(&1, %{prepared?: true}))}
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
      defaults [:read, :destroy, create: :*, update: :*]

      read :in_transaction do
        transaction? true
      end

      read :read_with_validation do
        argument :must_be_true, :boolean

        validate fn query, _ ->
          if query.arguments[:must_be_true] == true do
            :ok
          else
            {:error, field: :must_be_true, message: "must be true"}
          end
        end
      end

      read :read_with_action_is do
        argument :unused, :string
        validate action_is([:read_with_action_is])
      end

      read :read_with_argument_does_not_equal do
        argument :status, :string
        validate argument_does_not_equal(:status, "forbidden")
      end

      read :read_with_argument_equals do
        argument :mode, :string
        validate argument_equals(:mode, "active")
      end

      read :read_with_argument_in do
        argument :category, :string
        validate argument_in(:category, ["news", "sports", "tech"])
      end

      read :read_with_compare do
        argument :min_value, :integer
        argument :max_value, :integer
        validate compare(:min_value, less_than: :max_value)
      end

      read :read_with_confirm do
        argument :password, :string
        argument :password_confirmation, :string
        validate confirm(:password, :password_confirmation)
      end

      read :read_with_match do
        argument :email, :string
        validate match(:email, ~r/^[^\s]+@[^\s]+\.[^\s]+$/)
      end

      read :read_with_negate do
        argument :name, :string
        validate negate(argument_equals(:name, "banned"))
      end

      read :read_with_one_of do
        argument :priority, :string
        validate one_of(:priority, ["low", "medium", "high"])
      end

      read :read_with_present do
        argument :required_field, :string
        validate present([:required_field])
      end

      read :read_with_string_length do
        argument :username, :string
        validate string_length(:username, min: 3, max: 20)
      end

      # Tests for where clauses
      read :read_with_preparation_where do
        argument :should_prepare, :boolean, default: false
        argument :value, :string

        prepare set_context(%{prepared: true}) do
          where argument_equals(:should_prepare, true)
        end
      end

      read :read_with_validation_where do
        argument :validate_email, :boolean, default: false
        argument :email, :string

        validate match(:email, ~r/^[^\s]+@[^\s]+\.[^\s]+$/) do
          where argument_equals(:validate_email, true)
        end
      end

      # Tests for only_when_valid?
      read :read_with_only_when_valid do
        argument :required_arg, :string

        validate present([:required_arg])

        prepare set_context(%{preparation_ran: true}) do
          only_when_valid? true
        end
      end

      read :read_with_validation_only_when_valid do
        argument :value, :integer

        prepare set_context(%{test_only_when_valid: true})

        validate compare(:value, greater_than: 0)

        validate fn query, _ ->
          # This validation should only run if the query is valid
          if query.context[:test_only_when_valid] do
            :ok
          else
            {:error, field: :value, message: "test_only_when_valid not set"}
          end
        end do
          only_when_valid? true
        end
      end
    end

    attributes do
      uuid_primary_key(:id)
      attribute(:name, :string, public?: true)
    end

    relationships do
      has_many(:posts, Ash.Test.Actions.ReadTest.Post,
        destination_attribute: :author1_id,
        public?: true
      )
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    identities do
      identity(:backup_id, [:uuid], pre_check_with: Domain)
    end

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]

      read :not_paginatable

      read :read_with_after_action do
        prepare(PostPreparation)
      end

      read :read_with_authors do
        prepare(build(load: [:author1, :author2]))
      end

      read :read_with_unknown_intpus do
        skip_unknown_inputs :*
      end

      read :get_by_id do
        get_by(:id)
      end

      read :get_fred_by_id do
        get_by(:id)
        filter expr(title == "fred")
      end

      read :get_by_id_and_uuid do
        get_by([:id, :uuid])
      end
    end

    attributes do
      uuid_primary_key(:id)
      attribute(:uuid, :uuid, default: &Ash.UUID.generate/0, public?: true)
      attribute(:title, :string, public?: true)
      attribute(:contents, :string, public?: true)
    end

    relationships do
      belongs_to :author1, Ash.Test.Actions.ReadTest.Author do
        public?(true)
      end

      belongs_to :author2, Ash.Test.Actions.ReadTest.Author do
        public?(true)
      end
    end
  end

  describe "Ash.get/3" do
    setup do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "test", contents: "yeet"})
        |> Ash.create!()
        |> strip_metadata()

      %{post: post}
    end

    test "it returns a matching record", %{post: post} do
      assert {:ok, fetched_post} = Ash.get(Post, post.id)

      assert strip_metadata(fetched_post) == post
    end

    test "it returns an error when there is no matching record" do
      assert {:error, %Ash.Error.Invalid{errors: [%Ash.Error.Query.NotFound{}]}} =
               Ash.get(Post, Ash.UUID.generate())
    end

    test "it uses identities if they exist", %{post: post} do
      assert {:ok, fetched_post} = Ash.get(Post, uuid: post.uuid)

      assert strip_metadata(fetched_post) == post
    end

    test "raises an error when the first argument is not a module" do
      assert_raise ArgumentError,
                   "Expected an `Ash.Resource` in `Ash.get/3`, got: \"bogus\"",
                   fn -> Ash.get("bogus", 1, []) end
    end

    test "raises an error when the first argument is a module that is not an ash resource" do
      assert_raise ArgumentError,
                   "Expected an `Ash.Resource` in `Ash.get/3`, got: BadModuleName",
                   fn -> Ash.get(BadModuleName, []) end
    end

    test "raises an error when the third argument is not a list" do
      assert_raise ArgumentError, "Expected a keyword list in `Ash.get/3`, got: 1", fn ->
        Ash.get(Post, "id", 1)
      end
    end

    test "raises an error when the third argument is not a valid keyword list" do
      assert_raise ArgumentError, "Expected a keyword list in `Ash.get/3`, got: [1]", fn ->
        Ash.get(Post, "id", [1])
      end
    end
  end

  describe "around_transaction/2" do
    test "it runs around the action" do
      Post
      |> Ash.Changeset.for_create(:create, %{title: "test", contents: "yeet"})
      |> Ash.create!()
      |> strip_metadata()

      assert {:ok, [_]} =
               Post
               |> Ash.Query.around_transaction(fn query, callback ->
                 send(self(), :around_transaction_start)
                 {:ok, results} = callback.(query)
                 send(self(), :around_transaction_end)
                 {:ok, results}
               end)
               |> Ash.Query.before_action(fn query ->
                 send(self(), :before_action)
                 query
               end)
               |> Ash.read()

      assert {:messages, [:around_transaction_start, :before_action, :around_transaction_end]} =
               :erlang.process_info(self(), :messages)
    end
  end

  describe "Ash.get! with action" do
    setup do
      author1 =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "bruh"})
        |> Ash.create!()
        |> strip_metadata()

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "test", contents: "yeet"})
        |> Ash.Changeset.manage_relationship(:author1, author1, type: :append_and_remove)
        |> Ash.create!()

      %{post: post, author1: author1}
    end

    test "it uses the action provided", %{post: post, author1: author1} do
      fetched_post = Ash.get!(Post, post.id, action: :read_with_authors)
      assert ^author1 = strip_metadata(fetched_post.author1)
    end
  end

  describe "Ash.get!/3" do
    setup do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "test", contents: "yeet"})
        |> Ash.create!()
        |> strip_metadata()

      %{post: post}
    end

    test "it returns a matching record", %{post: post} do
      assert ^post = strip_metadata(Ash.get!(Post, post.id))
    end

    test "it gives an invalid primary key error when invalid input is provided" do
      assert_raise Ash.Error.Invalid, ~r/invalid primary key "not good"/, fn ->
        Ash.get!(Post, "not good")
      end
    end

    test "it raises when there is no matching record" do
      res =
        assert_raise Ash.Error.Invalid, fn ->
          Ash.get!(Post, Ash.UUID.generate())
        end

      assert [%Ash.Error.Query.NotFound{}] = res.errors
    end

    test "raises an error when the first argument is not a module", %{post: post} do
      assert_raise ArgumentError,
                   "Expected an `Ash.Resource` in `Ash.get/3`, got: \"bogus\"",
                   fn -> Ash.get("bogus", post.id, []) end
    end

    test "raises an error when the first argument is a module that is not an ash resource", %{
      post: post
    } do
      assert_raise ArgumentError,
                   "Expected an `Ash.Resource` in `Ash.get/3`, got: BadModuleName",
                   fn ->
                     Ash.get(BadModuleName, post.id, [])
                   end
    end

    test "raises an error when the third argument is not a list", %{post: post} do
      assert_raise ArgumentError, "Expected a keyword list in `Ash.get\/3`, got: 1", fn ->
        Ash.get(Post, post.id, 1)
      end
    end

    test "raises an error when the third argument is not a valid keyword list", %{post: post} do
      assert_raise ArgumentError, "Expected a keyword list in `Ash.get/3`, got: [1]", fn ->
        Ash.get(Post, post.id, [1])
      end
    end
  end

  describe "Ash.read/2 with no records" do
    test "returns an empty result" do
      assert {:ok, []} = Ash.read(Post)
    end

    test "raises an error when the first argument is not a module" do
      assert_raise ArgumentError,
                   "Expected an `%Ash.Query{}` or an `Ash.Resource` in `Ash.read/2`, got: \"bogus\"",
                   fn -> Ash.read("bogus", []) end
    end

    test "raises an error when the first argument is a module that is not an ash resource" do
      assert_raise ArgumentError,
                   "Expected an `%Ash.Query{}` or an `Ash.Resource` in `Ash.read\/2`, got: BadModuleName",
                   fn -> Ash.read(BadModuleName, []) end
    end

    test "raises an error when the second argument is not a list" do
      assert_raise ArgumentError, "Expected a keyword list in `Ash.read\/2`, got: 1", fn ->
        Ash.read(Post, 1)
      end
    end

    test "raises an error when the second argument is not a valid keyword list" do
      assert_raise ArgumentError, "Expected a keyword list in `Ash.read\/2`, got: \[1\]", fn ->
        Ash.read(Post, [1])
      end
    end
  end

  describe "transaction true" do
    test "with no records, returns an empty set" do
      assert [] = Ash.read!(Author, action: :in_transaction)
    end
  end

  describe "data_layer_query/2" do
    test "returns the data layer query and can be run" do
      %{run: run, query: query} = Ash.data_layer_query!(Post)
      assert {:ok, []} = run.(query)

      Post
      |> Ash.Changeset.for_create(:create, %{title: "test", contents: "yeet"})
      |> Ash.create!()

      assert {:ok, [_]} = run.(query)
    end
  end

  describe "Ash.read!/2 with no records" do
    test "returns an empty result" do
      assert [] = Ash.read!(Post)
    end

    test "raises an error when the first argument is not a module" do
      assert_raise ArgumentError,
                   "Expected an `%Ash.Query{}` or an `Ash.Resource` in `Ash.read!/2`, got: \"bogus\"",
                   fn -> Ash.read!("bogus", []) end
    end

    test "raises an error when the first argument is a module that is not an ash resource" do
      assert_raise ArgumentError,
                   "Expected an `%Ash.Query{}` or an `Ash.Resource` in `Ash.read!/2`, got: BadModuleName",
                   fn -> Ash.read!(BadModuleName, []) end
    end

    test "raises an error when the second argument is not a list" do
      assert_raise ArgumentError, "Expected a keyword list in `Ash.read!/2`, got: 1", fn ->
        Ash.read!(Post, 1)
      end
    end

    test "raises an error when the second argument is not a valid keyword list" do
      assert_raise ArgumentError, "Expected a keyword list in `Ash.read!/2`, got: [1]", fn ->
        Ash.read!(Post, [1])
      end
    end

    test "raises an error when page is sent but pagination is not enabled on a resource" do
      res =
        assert_raise Ash.Error.Invalid, fn ->
          Ash.read!(Post, action: :not_paginatable, page: [limit: 10])
        end

      assert %Ash.Error.Invalid.ActionRequiresPagination{resource: Post, action: _} =
               hd(res.errors)
    end
  end

  describe "Ash.read/2" do
    setup do
      post1 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "test", contents: "yeet"})
        |> Ash.create!()

      post2 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "test1", contents: "yeet2"})
        |> Ash.create!()

      %{post1: post1, post2: post2}
    end

    test "with a limit of 1, returns only 1 record" do
      assert {:ok, [_post]} =
               Post
               |> Ash.Query.limit(1)
               |> Ash.read()
    end

    test "return_query returns the query" do
      assert {:ok, [_post], %Ash.Query{limit: 1}} =
               Post
               |> Ash.Query.limit(1)
               |> Ash.read(return_query?: true)

      assert {:ok, %{}, %Ash.Query{limit: 1}} =
               Post
               |> Ash.read_first(return_query?: true)

      assert {:ok, %{}, %Ash.Query{limit: 1}} =
               Post
               |> Ash.Query.limit(1)
               |> Ash.read_one(return_query?: true)
    end

    test "after action hooks are run" do
      assert [%{__metadata__: %{prepared?: true}}, %{__metadata__: %{prepared?: true}}] =
               Ash.read!(Post, action: :read_with_after_action)
    end

    test "with a limit size of 2, returns 2 records" do
      assert {:ok, [_, _]} =
               Post
               |> Ash.Query.limit(2)
               |> Ash.read()
    end

    test "with a limit of 1 and an offset of 1, it returns 1 record" do
      assert {:ok, [_]} =
               Post
               |> Ash.Query.limit(1)
               |> Ash.Query.offset(1)
               |> Ash.read()
    end
  end

  describe "Ash.read!/2" do
    setup do
      post1 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "test", contents: "yeet"})
        |> Ash.create!()

      post2 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "test1", contents: "yeet2"})
        |> Ash.create!()

      %{post1: post1, post2: post2}
    end

    test "it returns the records not in a tuple" do
      assert [_, _] = Ash.read!(Post)
    end
  end

  describe "Ash.read_one/2" do
    test "raises an error when the first argument is not a module" do
      assert_raise ArgumentError,
                   "Expected an `%Ash.Query{}` or an `Ash.Resource` in `Ash.read_one/2`, got: \"bogus\"",
                   fn -> Ash.read_one("bogus", []) end
    end

    test "raises an error when the first argument is a module that is not an ash resource" do
      assert_raise ArgumentError,
                   "Expected an `%Ash.Query{}` or an `Ash.Resource` in `Ash.read_one/2`, got: BadModuleName",
                   fn -> Ash.read_one(BadModuleName, []) end
    end

    test "raises an error when the second argument is not a list" do
      assert_raise ArgumentError, "Expected a keyword list in `Ash.read_one/2`, got: 1", fn ->
        Ash.read_one(Post, 1)
      end
    end

    test "raises an error when the second argument is not a valid keyword list" do
      assert_raise ArgumentError, "Expected a keyword list in `Ash.read_one/2`, got: [1]", fn ->
        Ash.read_one(Post, [1])
      end
    end

    test "it applies a limit" do
      Ash.create!(Ash.Changeset.for_create(Post, :create, %{}, authorize?: false))
      Ash.create!(Ash.Changeset.for_create(Post, :create, %{}, authorize?: false))
      Ash.create!(Ash.Changeset.for_create(Post, :create, %{}, authorize?: false))
      assert %Post{} = Ash.read_one!(Post |> Ash.Query.limit(1))
    end
  end

  describe "Ash.read_one!/2" do
    test "raises an error when the first argument is not a module" do
      assert_raise ArgumentError,
                   "Expected an `%Ash.Query{}` or an `Ash.Resource` in `Ash.read_one!\/2`, got: \"bogus\"",
                   fn -> Ash.read_one!("bogus", []) end
    end

    test "raises an error when the first argument is a module that is not an ash resource" do
      assert_raise ArgumentError,
                   "Expected an `%Ash.Query{}` or an `Ash.Resource` in `Ash.read_one!\/2`, got: BadModuleName",
                   fn -> Ash.read_one!(BadModuleName, []) end
    end

    test "raises an error when the second argument is not a list" do
      assert_raise ArgumentError, "Expected a keyword list in `Ash.read_one!/2`, got: 1", fn ->
        Ash.read_one!(Post, 1)
      end
    end

    test "raises an error when the second argument is not a valid keyword list" do
      assert_raise ArgumentError, "Expected a keyword list in `Ash.read_one!/2`, got: [1]", fn ->
        Ash.read_one!(Post, [1])
      end
    end
  end

  describe "filters" do
    setup do
      post1 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "test", contents: "yeet"})
        |> Ash.create!()
        |> strip_metadata()

      post2 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "test1", contents: "yeet"})
        |> Ash.create!()
        |> strip_metadata()

      %{post1: post1, post2: post2}
    end

    test "a filter that matches nothing returns no results" do
      assert {:ok, []} =
               Post
               |> Ash.Query.filter(contents == "not_yeet")
               |> Ash.read()
    end

    test "a filter returns only matching records", %{post1: post1} do
      assert {:ok, [^post1]} =
               Post
               |> Ash.Query.filter(title == ^post1.title)
               |> Ash.read()
               |> strip_metadata()
    end

    test "a filter returns multiple records if they match", %{post1: post1, post2: post2} do
      assert {:ok, [_, _] = results} =
               Post
               |> Ash.Query.filter(contents == "yeet")
               |> Ash.read()
               |> strip_metadata()

      assert post1 in results
      assert post2 in results
    end
  end

  describe "select" do
    test "it automatically selects all fields" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "bruh"})
        |> Ash.create!()

      assert author.name
      assert author.id
    end

    test "you can deselect a field" do
      Author
      |> Ash.Changeset.for_create(:create, %{name: "bruh"})
      |> Ash.create!()

      assert [%{name: "bruh"}] = Ash.read!(Author)

      assert [%{name: %Ash.NotLoaded{}}] = Ash.read!(Ash.Query.deselect(Author, :name))
    end

    test "deselected fields don't return nil" do
      Author
      |> Ash.Changeset.for_create(:create, %{name: "bruh"})
      |> Ash.create!()

      assert [%{name: "bruh"}] = Ash.read!(Author)

      assert [%{name: %Ash.NotLoaded{field: :name}}] =
               Ash.read!(Ash.Query.deselect(Author, :name))
    end

    test "you can select fields, but the primary key is always present" do
      Author
      |> Ash.Changeset.for_create(:create, %{name: "bruh"})
      |> Ash.create!()

      assert [%{name: "bruh", id: id}] = Ash.read!(Ash.Query.select(Author, :name))
      assert id
    end
  end

  describe "validations" do
    test "passing validations produce no errors" do
      assert [] =
               Author
               |> Ash.Query.for_read(:read_with_validation, %{must_be_true: true})
               |> Ash.read!()
    end

    test "failing validations produce errors" do
      assert_raise Ash.Error.Invalid, ~r/must be true/, fn ->
        Author
        |> Ash.Query.for_read(:read_with_validation, %{must_be_true: false})
        |> Ash.read!()
      end
    end
  end

  describe "relationship filters" do
    setup do
      author1 =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "bruh"})
        |> Ash.create!()

      author2 =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "bruh"})
        |> Ash.create!()

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "test", contents: "yeet"})
        |> Ash.Changeset.manage_relationship(:author1, author1, type: :append_and_remove)
        |> Ash.Changeset.manage_relationship(:author2, author2, type: :append_and_remove)
        |> Ash.create!()

      %{post: post, author1: author1, author2: author2}
    end

    test "you can filter on a related value", %{author1: author1} do
      assert [_] =
               Post
               |> Ash.Query.filter(author1: author1.id)
               |> Ash.read!()
    end

    test "you can filter on multiple related values", %{author1: author1, author2: author2} do
      assert [_] =
               Post
               |> Ash.Query.filter(author1: author1.id, author2: author2.id)
               |> Ash.read!()
    end
  end

  describe "sort" do
    setup do
      post1 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "abc", contents: "abc"})
        |> Ash.create!()
        |> strip_metadata()

      post2 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "xyz", contents: "abc"})
        |> Ash.create!()
        |> strip_metadata()

      %{post1: post1, post2: post2}
    end

    test "a sort will sort the rows accordingly when ascending", %{
      post1: post1,
      post2: post2
    } do
      assert {:ok, [^post1, ^post2]} =
               Post
               |> Ash.Query.sort(title: :asc)
               |> Ash.read()
               |> strip_metadata()
    end

    test "a sort will sort rows accordingly when descending", %{
      post1: post1,
      post2: post2
    } do
      assert {:ok, [^post2, ^post1]} =
               Post
               |> Ash.Query.sort(title: :desc)
               |> Ash.read()
               |> strip_metadata()
    end

    test "a nested sort sorts accordingly", %{post1: post1, post2: post2} do
      middle_post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "abc", contents: "xyz"})
        |> Ash.create!()
        |> strip_metadata()

      assert {:ok, [^post1, ^middle_post, ^post2]} =
               Post
               |> Ash.Query.sort(title: :asc, contents: :asc)
               |> Ash.read()
               |> strip_metadata()
    end

    test "a sort can use an expression" do
      require Ash.Sort

      Post
      |> Ash.Query.sort([{Ash.Sort.expr_sort(title <> contents), :asc}])
      |> Ash.read!()
    end
  end

  describe "get_by with only a single field" do
    setup do
      post =
        Enum.map(0..2, fn _ ->
          Post
          |> Ash.Changeset.for_create(:create, %{title: "test", contents: "yeet"})
          |> Ash.create!()
          |> strip_metadata()
        end)
        |> Enum.random()

      %{post_id: post.id}
    end

    test "it succeeds when the record exists", %{post_id: post_id} do
      assert {:ok, %{id: ^post_id}} =
               Post |> Ash.Query.for_read(:get_by_id, %{id: post_id}) |> Ash.read_one()
    end

    test "it composes with existing filters", %{post_id: post_id} do
      assert {:ok, nil} =
               Post |> Ash.Query.for_read(:get_fred_by_id, %{id: post_id}) |> Ash.read_one()

      assert %{id: freds_id} =
               Post
               |> Ash.Changeset.for_create(:create, %{title: "fred", contents: "yeet"})
               |> Ash.create!()
               |> strip_metadata()

      assert {:ok, %{id: ^freds_id}} =
               Post |> Ash.Query.for_read(:get_fred_by_id, %{id: freds_id}) |> Ash.read_one()
    end

    test "it fails when the record does not exist" do
      assert {:ok, nil} =
               Post
               |> Ash.Query.for_read(:get_by_id, %{id: Ash.UUID.generate()})
               |> Ash.read_one()
    end
  end

  describe "get_by with multiple fields" do
    setup do
      post =
        Enum.map(0..2, fn _ ->
          Post
          |> Ash.Changeset.for_create(:create, %{title: "test", contents: "yeet"})
          |> Ash.create!()
          |> strip_metadata()
        end)
        |> Enum.random()

      %{post_id: post.id, post_uuid: post.uuid}
    end

    test "it succeeds when the record exists", %{post_id: post_id, post_uuid: post_uuid} do
      assert {:ok, %{id: ^post_id, uuid: ^post_uuid}} =
               Post
               |> Ash.Query.for_read(:get_by_id_and_uuid, %{id: post_id, uuid: post_uuid})
               |> Ash.read_one()
    end

    test "it fails when the record does not exist" do
      assert {:ok, nil} =
               Post
               |> Ash.Query.for_read(:get_by_id_and_uuid, %{
                 id: Ash.UUID.generate(),
                 uuid: Ash.UUID.generate()
               })
               |> Ash.read_one()
    end
  end

  describe "skip_unknown_inputs" do
    test "with opts from action" do
      assert {:ok, []} =
               Post
               |> Ash.Query.for_read(:read_with_unknown_intpus, %{unknown: "input"})
               |> Ash.read()
    end

    test "with opts from query" do
      # opts from query override opts from action
      assert {:error,
              %Ash.Error.Invalid{errors: [%Ash.Error.Invalid.NoSuchInput{input: :unknown}]}} =
               Post
               |> Ash.Query.for_read(:read_with_unknown_intpus, %{unknown: "input"},
                 skip_unknown_inputs: []
               )
               |> Ash.read()

      assert {:ok, []} =
               Post
               |> Ash.Query.for_read(:read_with_unknown_intpus, %{unknown: "input"},
                 skip_unknown_inputs: [:unknown]
               )
               |> Ash.read()
    end
  end

  describe "tenant metadata in before_action hooks" do
    defmodule TenantPost do
      @moduledoc false
      use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

      ets do
        private?(true)
      end

      multitenancy do
        strategy(:attribute)
        attribute(:org_id)
      end

      actions do
        default_accept :*
        defaults [:read, :destroy, create: :*, update: :*]

        read :read_with_tenant_hook do
          prepare fn query, _opts ->
            Ash.Query.before_action(query, fn query ->
              # Modify the tenant in the before_action hook
              # This simulates cases where tenant might be dynamically determined
              original_tenant = query.tenant
              modified_tenant = "modified-#{original_tenant}"
              Ash.Query.set_tenant(query, modified_tenant)
            end)
          end
        end

        read :bypass_and_set_tenant do
          multitenancy(:bypass)

          prepare fn query, _opts ->
            Ash.Query.before_action(query, fn query ->
              # Dynamically set tenant in before_action hook
              Ash.Query.set_tenant(query, "dynamic-tenant")
            end)
          end
        end
      end

      attributes do
        uuid_primary_key(:id)
        attribute(:title, :string, public?: true)
        attribute(:contents, :string, public?: true)
        attribute(:org_id, :uuid, public?: true)
      end
    end

    setup do
      %{tenant1: Ash.UUID.generate(), tenant2: Ash.UUID.generate()}
    end

    test "tenant modified in before_action hook appears in record metadata", %{tenant1: tenant1} do
      TenantPost
      |> Ash.Changeset.for_create(:create, %{title: "test", contents: "yeet"}, tenant: tenant1)
      |> Ash.create!()

      # Read with the action that modifies tenant in before_action hook
      [result] =
        TenantPost
        |> Ash.Query.set_tenant(tenant1)
        |> Ash.read!(action: :read_with_tenant_hook)

      assert result.__metadata__.tenant == "modified-#{tenant1}"
    end

    test "tenant set in before_action hook with bypass works correctly", %{tenant1: tenant1} do
      TenantPost
      |> Ash.Changeset.for_create(:create, %{title: "test", contents: "yeet"}, tenant: tenant1)
      |> Ash.create!()

      # Read with the action that bypasses tenant requirements but sets tenant in before_action hook
      [result] =
        TenantPost
        |> Ash.read!(action: :bypass_and_set_tenant)

      assert result.__metadata__.tenant == "dynamic-tenant"
    end

    test "after_transaction hook runs when multitenancy check fails", %{tenant1: tenant1} do
      TenantPost
      |> Ash.Changeset.for_create(:create, %{title: "test", contents: "yeet"}, tenant: tenant1)
      |> Ash.create!()

      query =
        TenantPost
        |> Ash.Query.after_transaction(fn _query, {:error, %Ash.Error.Invalid.TenantRequired{}} ->
          {:error, "Custom error from after_transaction"}
        end)

      assert {:error, error} = Ash.read(query)
      assert Exception.message(error) =~ "Custom error from after_transaction"
    end
  end

  describe "query validations" do
    test "action_is validation passes when action matches" do
      assert [] =
               Author
               |> Ash.Query.for_read(:read_with_action_is, %{unused: "test"})
               |> Ash.read!()
    end

    test "argument_does_not_equal validation passes when argument is different" do
      assert [] =
               Author
               |> Ash.Query.for_read(:read_with_argument_does_not_equal, %{status: "active"})
               |> Ash.read!()
    end

    test "argument_does_not_equal validation fails when argument equals forbidden value" do
      assert_raise Ash.Error.Invalid, ~r/must not equal/, fn ->
        Author
        |> Ash.Query.for_read(:read_with_argument_does_not_equal, %{status: "forbidden"})
        |> Ash.read!()
      end
    end

    test "argument_equals validation passes when argument matches" do
      assert [] =
               Author
               |> Ash.Query.for_read(:read_with_argument_equals, %{mode: "active"})
               |> Ash.read!()
    end

    test "argument_equals validation fails when argument doesn't match" do
      assert_raise Ash.Error.Invalid, ~r/must equal/, fn ->
        Author
        |> Ash.Query.for_read(:read_with_argument_equals, %{mode: "inactive"})
        |> Ash.read!()
      end
    end

    test "argument_in validation passes when argument is in list" do
      assert [] =
               Author
               |> Ash.Query.for_read(:read_with_argument_in, %{category: "news"})
               |> Ash.read!()
    end

    test "argument_in validation fails when argument is not in list" do
      assert_raise Ash.Error.Invalid, ~r/must equal/, fn ->
        Author
        |> Ash.Query.for_read(:read_with_argument_in, %{category: "invalid"})
        |> Ash.read!()
      end
    end

    test "compare validation passes when comparison is satisfied" do
      assert [] =
               Author
               |> Ash.Query.for_read(:read_with_compare, %{min_value: 5, max_value: 10})
               |> Ash.read!()
    end

    test "compare validation fails when comparison is not satisfied" do
      assert_raise Ash.Error.Invalid, ~r/must be less than/, fn ->
        Author
        |> Ash.Query.for_read(:read_with_compare, %{min_value: 15, max_value: 10})
        |> Ash.read!()
      end
    end

    test "confirm validation passes when fields match" do
      assert [] =
               Author
               |> Ash.Query.for_read(:read_with_confirm, %{
                 password: "secret",
                 password_confirmation: "secret"
               })
               |> Ash.read!()
    end

    test "confirm validation fails when fields don't match" do
      assert_raise Ash.Error.Invalid, ~r/confirmation did not match/, fn ->
        Author
        |> Ash.Query.for_read(:read_with_confirm, %{
          password: "secret",
          password_confirmation: "different"
        })
        |> Ash.read!()
      end
    end

    test "match validation passes when argument matches regex" do
      assert [] =
               Author
               |> Ash.Query.for_read(:read_with_match, %{email: "test@example.com"})
               |> Ash.read!()
    end

    test "match validation fails when argument doesn't match regex" do
      assert_raise Ash.Error.Invalid, ~r/must match/, fn ->
        Author
        |> Ash.Query.for_read(:read_with_match, %{email: "invalid-email"})
        |> Ash.read!()
      end
    end

    test "negate validation passes when negated validation fails" do
      assert [] =
               Author
               |> Ash.Query.for_read(:read_with_negate, %{name: "allowed"})
               |> Ash.read!()
    end

    test "negate validation fails when negated validation passes" do
      assert_raise Ash.Error.Invalid, ~r/must not pass validation/, fn ->
        Author
        |> Ash.Query.for_read(:read_with_negate, %{name: "banned"})
        |> Ash.read!()
      end
    end

    test "one_of validation passes when argument is in allowed values" do
      assert [] =
               Author
               |> Ash.Query.for_read(:read_with_one_of, %{priority: "high"})
               |> Ash.read!()
    end

    test "one_of validation fails when argument is not in allowed values" do
      assert_raise Ash.Error.Invalid, ~r/expected one of/, fn ->
        Author
        |> Ash.Query.for_read(:read_with_one_of, %{priority: "urgent"})
        |> Ash.read!()
      end
    end

    test "present validation passes when required field is present" do
      assert [] =
               Author
               |> Ash.Query.for_read(:read_with_present, %{required_field: "value"})
               |> Ash.read!()
    end

    test "present validation fails when required field is missing" do
      assert_raise Ash.Error.Invalid, ~r/must be present/, fn ->
        Author
        |> Ash.Query.for_read(:read_with_present, %{})
        |> Ash.read!()
      end
    end

    test "string_length validation passes when string is within bounds" do
      assert [] =
               Author
               |> Ash.Query.for_read(:read_with_string_length, %{username: "john"})
               |> Ash.read!()
    end

    test "string_length validation fails when string is too short" do
      assert_raise Ash.Error.Invalid, ~r/must have length of between/, fn ->
        Author
        |> Ash.Query.for_read(:read_with_string_length, %{username: "ab"})
        |> Ash.read!()
      end
    end

    test "string_length validation fails when string is too long" do
      assert_raise Ash.Error.Invalid, ~r/must have length of between/, fn ->
        Author
        |> Ash.Query.for_read(:read_with_string_length, %{
          username: "this_username_is_way_too_long_to_be_valid"
        })
        |> Ash.read!()
      end
    end
  end

  describe "preparations with where clauses" do
    test "preparation runs when where clause is satisfied" do
      query =
        Author
        |> Ash.Query.for_read(:read_with_preparation_where, %{
          should_prepare: true,
          value: "test"
        })

      assert query.context[:prepared] == true
    end

    test "preparation does not run when where clause is not satisfied" do
      query =
        Author
        |> Ash.Query.for_read(:read_with_preparation_where, %{
          should_prepare: false,
          value: "test"
        })

      refute Map.has_key?(query.context, :prepared)
    end
  end

  describe "validations with where clauses" do
    test "validation runs when where clause is satisfied" do
      # Valid email should pass
      assert [] =
               Author
               |> Ash.Query.for_read(:read_with_validation_where, %{
                 validate_email: true,
                 email: "test@example.com"
               })
               |> Ash.read!()

      # Invalid email should fail
      assert_raise Ash.Error.Invalid, ~r/must match/, fn ->
        Author
        |> Ash.Query.for_read(:read_with_validation_where, %{
          validate_email: true,
          email: "not-an-email"
        })
        |> Ash.read!()
      end
    end

    test "validation does not run when where clause is not satisfied" do
      # Invalid email should still pass because validation doesn't run
      assert [] =
               Author
               |> Ash.Query.for_read(:read_with_validation_where, %{
                 validate_email: false,
                 email: "not-an-email"
               })
               |> Ash.read!()
    end
  end

  describe "only_when_valid? option" do
    test "preparation with only_when_valid? runs when query is valid" do
      query =
        Author
        |> Ash.Query.for_read(:read_with_only_when_valid, %{
          required_arg: "provided"
        })

      assert query.context[:preparation_ran] == true
    end

    test "preparation with only_when_valid? does not run when query is invalid" do
      # This will fail validation, so the preparation should not run
      assert_raise Ash.Error.Invalid, ~r/must be present/, fn ->
        Author
        |> Ash.Query.for_read(:read_with_only_when_valid, %{})
        |> Ash.read!()
      end
    end

    test "validation with only_when_valid? runs when earlier validations pass" do
      # Valid value (> 0) should pass all validations
      assert [] =
               Author
               |> Ash.Query.for_read(:read_with_validation_only_when_valid, %{
                 value: 5
               })
               |> Ash.read!()
    end

    test "validation with only_when_valid? does not run when earlier validations fail" do
      # Value <= 0 should fail the first validation
      # The second validation with only_when_valid? should not run
      # So we should get the "greater than" error, not the "test_only_when_valid not set" error
      assert_raise Ash.Error.Invalid, ~r/must be greater than/, fn ->
        Author
        |> Ash.Query.for_read(:read_with_validation_only_when_valid, %{
          value: 0
        })
        |> Ash.read!()
      end
    end
  end

  defmodule GlobalValidationRead do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

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

      read :skip_global do
        skip_global_validations? true
        argument :name, :string
      end

      read :with_global do
        argument :name, :string
      end
    end

    validations do
      validate present(:name), on: [:read]
    end
  end

  describe "global validations on read actions" do
    test "global validations run on read actions by default" do
      assert_raise Ash.Error.Invalid, ~r/must be present/, fn ->
        GlobalValidationRead
        |> Ash.Query.for_read(:with_global, %{})
        |> Ash.read!()
      end

      # Should pass when argument is provided
      assert [] =
               GlobalValidationRead
               |> Ash.Query.for_read(:with_global, %{name: "test"})
               |> Ash.read!()
    end

    test "global validations can be skipped on read actions" do
      # Should pass even without required argument when global validations are skipped
      assert [] =
               GlobalValidationRead
               |> Ash.Query.for_read(:skip_global, %{})
               |> Ash.read!()
    end
  end

  describe "transaction hooks" do
    test "after_transaction hook error overrides success result" do
      Author |> Ash.Changeset.for_create(:create, %{name: "Test"}) |> Ash.create!()

      query =
        Author
        |> Ash.Query.filter(name == "Test")
        |> Ash.Query.after_transaction(fn _query, {:ok, _results} ->
          {:error, "Custom error from after_transaction hook"}
        end)

      assert_raise Ash.Error.Unknown, ~r/Custom error from after_transaction hook/, fn ->
        Ash.read!(query, action: :in_transaction)
      end
    end

    test "after_transaction hook error overrides forbidden error" do
      Author |> Ash.Changeset.for_create(:create, %{name: "Test"}) |> Ash.create!()

      query =
        Author
        |> Ash.Query.filter(name == "Test")
        |> Ash.Query.after_action(fn _query, _results ->
          {:error, Ash.Error.Forbidden.exception([])}
        end)
        |> Ash.Query.after_transaction(fn _query, {:error, %Ash.Error.Forbidden{}} ->
          {:error, "Custom error from after_transaction hook"}
        end)

      assert_raise Ash.Error.Unknown, ~r/Custom error from after_transaction hook/, fn ->
        Ash.read!(query, action: :in_transaction)
      end
    end

    test "before_transaction hook can modify query" do
      author1 = Author |> Ash.Changeset.for_create(:create, %{name: "Test"}) |> Ash.create!()
      _author2 = Author |> Ash.Changeset.for_create(:create, %{name: "Other"}) |> Ash.create!()

      query =
        Author
        |> Ash.Query.before_transaction(fn query ->
          Ash.Query.filter(query, name == "Test")
        end)

      results = Ash.read!(query, action: :in_transaction)

      assert length(results) == 1
      assert List.first(results).id == author1.id
    end

    test "before_transaction hook can return an error" do
      query =
        Author
        |> Ash.Query.before_transaction(fn _query ->
          {:error, "Before transaction error"}
        end)

      assert_raise Ash.Error.Unknown, ~r/Before transaction error/, fn ->
        Ash.read!(query, action: :in_transaction)
      end
    end

    test "multiple before_transaction hooks run in order" do
      agent = start_supervised!({Agent, fn -> [] end})

      query =
        Author
        |> Ash.Query.before_transaction(fn query ->
          Agent.update(agent, &["first" | &1])
          query
        end)
        |> Ash.Query.before_transaction(fn query ->
          Agent.update(agent, &["second" | &1])
          query
        end)

      Ash.read!(query, action: :in_transaction)

      assert Agent.get(agent, & &1) == ["second", "first"]
    end

    test "after_transaction hook receives query and result" do
      author =
        Author |> Ash.Changeset.for_create(:create, %{name: "Test"}) |> Ash.create!()

      agent = start_supervised!({Agent, fn -> nil end})

      query =
        Author
        |> Ash.Query.filter(name == "Test")
        |> Ash.Query.after_transaction(fn query, result ->
          Agent.update(agent, fn _ -> {query.resource, result} end)
          result
        end)

      author_id = author.id
      [%{id: ^author_id}] = Ash.read!(query, action: :in_transaction)
      {resource, result_tuple} = Agent.get(agent, & &1)
      assert resource == Author
      assert {:ok, [%{id: ^author_id}]} = result_tuple
    end

    test "after_transaction hook can modify the result" do
      Author |> Ash.Changeset.for_create(:create, %{name: "Test"}) |> Ash.create!()

      query =
        Author
        |> Ash.Query.filter(name == "Test")
        |> Ash.Query.after_transaction(fn _query, {:ok, results} ->
          modified_results = Enum.map(results, &Map.put(&1, :__metadata__, :modified))
          {:ok, modified_results}
        end)

      results = Ash.read!(query, action: :in_transaction)
      assert List.first(results).__metadata__ == :modified
    end

    test "after_transaction hook runs on error" do
      agent = start_supervised!({Agent, fn -> nil end})

      query =
        Author
        |> Ash.Query.before_transaction(fn _query ->
          {:error, "Intentional error"}
        end)
        |> Ash.Query.after_transaction(fn _query, result ->
          Agent.update(agent, fn _ -> result end)
          result
        end)

      assert_raise Ash.Error.Unknown, fn ->
        Ash.read!(query, action: :in_transaction)
      end

      result = Agent.get(agent, & &1)
      assert {:error, _} = result
    end

    test "after_transaction hook gets correct result with after_action error" do
      agent = start_supervised!({Agent, fn -> nil end})

      query =
        Author
        |> Ash.Query.after_action(fn _query, _records ->
          {:error, "Intentional error"}
        end)
        |> Ash.Query.after_transaction(fn _query, result ->
          Agent.update(agent, fn _ -> result end)
          result
        end)

      assert_raise Ash.Error.Unknown, fn ->
        Ash.read!(query, action: :in_transaction)
      end

      result = Agent.get(agent, & &1)
      assert {:error, _} = result
    end

    test "multiple after_transaction hooks run in order" do
      Author |> Ash.Changeset.for_create(:create, %{name: "Test"}) |> Ash.create!()
      agent = start_supervised!({Agent, fn -> [] end})

      query =
        Author
        |> Ash.Query.filter(name == "Test")
        |> Ash.Query.after_transaction(fn _query, result ->
          Agent.update(agent, &["first" | &1])
          result
        end)
        |> Ash.Query.after_transaction(fn _query, result ->
          Agent.update(agent, &["second" | &1])
          result
        end)

      Ash.read!(query, action: :in_transaction)

      assert Agent.get(agent, & &1) == ["second", "first"]
    end

    test "hooks run in correct order: around(start) -> before -> action -> after -> around(end)" do
      Author |> Ash.Changeset.for_create(:create, %{name: "Test"}) |> Ash.create!()
      agent = start_supervised!({Agent, fn -> [] end})

      query =
        Author
        |> Ash.Query.filter(name == "Test")
        |> Ash.Query.around_transaction(fn query, callback ->
          Agent.update(agent, &["around_start" | &1])
          result = callback.(query)
          Agent.update(agent, &["around_end" | &1])
          result
        end)
        |> Ash.Query.before_transaction(fn query ->
          Agent.update(agent, &["before" | &1])
          query
        end)
        |> Ash.Query.after_transaction(fn _query, result ->
          Agent.update(agent, &["after" | &1])
          result
        end)

      Ash.read!(query, action: :in_transaction)

      assert Agent.get(agent, & &1) == ["around_end", "after", "before", "around_start"]
    end

    test "error in before_transaction still runs after_transaction and around_transaction end" do
      agent = start_supervised!({Agent, fn -> [] end})

      query =
        Author
        |> Ash.Query.around_transaction(fn query, callback ->
          Agent.update(agent, &["around_start" | &1])
          result = callback.(query)
          Agent.update(agent, &["around_end" | &1])
          result
        end)
        |> Ash.Query.before_transaction(fn _query ->
          Agent.update(agent, &["before_error" | &1])
          {:error, "Before transaction error"}
        end)
        |> Ash.Query.after_transaction(fn _query, result ->
          Agent.update(agent, &["after" | &1])
          result
        end)

      assert_raise Ash.Error.Unknown, fn ->
        Ash.read!(query, action: :in_transaction)
      end

      assert Agent.get(agent, & &1) == ["around_end", "after", "before_error", "around_start"]
    end

    test "before_transaction hook must return valid value" do
      query =
        Author
        |> Ash.Query.before_transaction(fn _query ->
          "invalid return value"
        end)

      assert_raise Ash.Error.Unknown, ~r/Invalid return value from before_transaction hook/, fn ->
        Ash.read!(query, action: :in_transaction)
      end
    end
  end
end
