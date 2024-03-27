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

      read :get_by_id do
        get_by(:id)
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

    test "a sort will sor rows accordingly when descending", %{
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
end
