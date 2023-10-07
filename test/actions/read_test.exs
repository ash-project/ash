defmodule Ash.Test.Actions.ReadTest do
  @moduledoc false
  use ExUnit.Case, async: true

  import Ash.Changeset
  import Ash.Test

  require Ash.Query

  alias Ash.Test.AnyApi, as: Api

  defmodule PostPreparation do
    @moduledoc false
    use Ash.Resource.Preparation

    def prepare(query, _, _) do
      Ash.Query.after_action(query, fn _query, authors ->
        {:ok, Enum.map(authors, &Ash.Resource.set_metadata(&1, %{prepared?: true}))}
      end)
    end
  end

  defmodule Author do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      defaults([:read, :create, :update, :destroy])
    end

    attributes do
      uuid_primary_key(:id)
      attribute(:name, :string)
    end

    relationships do
      has_many(:posts, Ash.Test.Actions.ReadTest.Post, destination_attribute: :author1_id)
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    identities do
      identity(:backup_id, [:uuid], pre_check_with: Api)
    end

    ets do
      private?(true)
    end

    actions do
      defaults([:read, :create, :update, :destroy])

      read :read_with_after_action do
        prepare(PostPreparation)
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
      attribute(:uuid, :uuid, default: &Ash.UUID.generate/0)
      attribute(:title, :string)
      attribute(:contents, :string)
    end

    relationships do
      belongs_to(:author1, Ash.Test.Actions.ReadTest.Author)
      belongs_to(:author2, Ash.Test.Actions.ReadTest.Author)
    end
  end

  describe "api.get/3" do
    setup do
      post =
        Post
        |> new(%{title: "test", contents: "yeet"})
        |> Api.create!()
        |> strip_metadata()

      %{post: post}
    end

    test "it returns a matching record", %{post: post} do
      assert {:ok, fetched_post} = Api.get(Post, post.id)

      assert strip_metadata(fetched_post) == post
    end

    test "it returns an error when there is no matching record" do
      assert {:error, %Ash.Error.Invalid{errors: [%Ash.Error.Query.NotFound{}]}} =
               Api.get(Post, Ash.UUID.generate())
    end

    test "it uses identities if they exist", %{post: post} do
      assert {:ok, fetched_post} = Api.get(Post, uuid: post.uuid)

      assert strip_metadata(fetched_post) == post
    end

    test "raises an error when the first argument is not a module" do
      res = assert_raise Ash.Error.Invalid.NoSuchResource, fn -> Api.get("bogus", 1, []) end
      assert res.message =~ ~r/Ash.Test.AnyApi.get\/3/
      assert res.message =~ ~r/expected an Ash Resource but instead got "bogus"/
    end

    test "raises an error when the first argument is a module that is not an ash resource" do
      res = assert_raise Ash.Error.Invalid.NoSuchResource, fn -> Api.get(BadModuleName, []) end
      assert res.message =~ ~r/Ash.Test.AnyApi.get\/3/
      assert res.message =~ ~r/expected an Ash Resource but instead got BadModuleName/
    end

    test "raises an error when the third argument is not a list" do
      res = assert_raise RuntimeError, fn -> Api.get(Post, "id", 1) end
      assert res.message =~ ~r/Ash.Test.AnyApi.get\/3/
      assert res.message =~ ~r/expected a keyword list, but instead got 1/
    end

    test "raises an error when the third argument is not a valid keyword list" do
      res = assert_raise RuntimeError, fn -> Api.get(Post, "id", [1]) end
      assert res.message =~ ~r/Ash.Test.AnyApi.get\/3/
      assert res.message =~ ~r/expected a keyword list, but instead got \[1\]/
    end
  end

  describe "around_transaction/2" do
    test "it runs around the action" do
      Post
      |> new(%{title: "test", contents: "yeet"})
      |> Api.create!()
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
               |> Api.read()

      assert {:messages, [:around_transaction_start, :before_action, :around_transaction_end]} =
               :erlang.process_info(self(), :messages)
    end
  end

  describe "api.get!/3" do
    setup do
      post =
        Post
        |> new(%{title: "test", contents: "yeet"})
        |> Api.create!()
        |> strip_metadata()

      %{post: post}
    end

    test "it returns a matching record", %{post: post} do
      assert ^post = strip_metadata(Api.get!(Post, post.id))
    end

    test "it gives an invalid primary key error when invalid input is provided" do
      assert_raise Ash.Error.Invalid, ~r/invalid primary key "not good"/, fn ->
        Api.get!(Post, "not good")
      end
    end

    test "it raises when there is no matching record" do
      res =
        assert_raise Ash.Error.Invalid, fn ->
          Api.get!(Post, Ash.UUID.generate())
        end

      assert [%Ash.Error.Query.NotFound{}] = res.errors
    end

    test "raises an error when the first argument is not a module", %{post: post} do
      res = assert_raise Ash.Error.Invalid.NoSuchResource, fn -> Api.get("bogus", post.id, []) end
      assert res.message =~ ~r/Ash.Test.AnyApi.get\/3/
      assert res.message =~ ~r/expected an Ash Resource but instead got "bogus"/
    end

    test "raises an error when the first argument is a module that is not an ash resource", %{
      post: post
    } do
      res =
        assert_raise Ash.Error.Invalid.NoSuchResource, fn ->
          Api.get(BadModuleName, post.id, [])
        end

      assert res.message =~ ~r/Ash.Test.AnyApi.get\/3/
      assert res.message =~ ~r/expected an Ash Resource but instead got BadModuleName/
    end

    test "raises an error when the third argument is not a list", %{post: post} do
      res = assert_raise RuntimeError, fn -> Api.get(Post, post.id, 1) end
      assert res.message =~ ~r/Ash.Test.AnyApi.get\/3/
      assert res.message =~ ~r/expected a keyword list, but instead got 1/
    end

    test "raises an error when the third argument is not a valid keyword list", %{post: post} do
      res = assert_raise RuntimeError, fn -> Api.get(Post, post.id, [1]) end
      assert res.message =~ ~r/Ash.Test.AnyApi.get\/3/
      assert res.message =~ ~r/expected a keyword list, but instead got \[1\]/
    end
  end

  describe "Api.read/2 with no records" do
    test "returns an empty result" do
      assert {:ok, []} = Api.read(Post)
    end

    test "raises an error when the first argument is not a module" do
      res = assert_raise RuntimeError, fn -> Api.read("bogus", []) end
      assert res.message =~ ~r/Ash.Test.AnyApi.read\/2/

      assert res.message =~
               ~r/expected an %Ash.Query{} or an Ash Resource but instead got "bogus"/
    end

    test "raises an error when the first argument is a module that is not an ash resource" do
      res = assert_raise RuntimeError, fn -> Api.read(BadModuleName, []) end
      assert res.message =~ ~r/Ash.Test.AnyApi.read\/2/

      assert res.message =~
               ~r/expected an %Ash.Query{} or an Ash Resource but instead got BadModuleName/
    end

    test "raises an error when the second argument is not a list" do
      res = assert_raise RuntimeError, fn -> Api.read(Post, 1) end
      assert res.message =~ ~r/Ash.Test.AnyApi.read\/2/
      assert res.message =~ ~r/expected a keyword list, but instead got 1/
    end

    test "raises an error when the second argument is not a valid keyword list" do
      res = assert_raise RuntimeError, fn -> Api.read(Post, [1]) end
      assert res.message =~ ~r/Ash.Test.AnyApi.read\/2/
      assert res.message =~ ~r/expected a keyword list, but instead got \[1\]/
    end
  end

  describe "Api.read!/2 with no records" do
    test "returns an empty result" do
      assert [] = Api.read!(Post)
    end

    test "raises an error when the first argument is not a module" do
      res = assert_raise RuntimeError, fn -> Api.read!("bogus", []) end
      assert res.message =~ ~r/Ash.Test.AnyApi.read!\/2/

      assert res.message =~
               ~r/expected an %Ash.Query{} or an Ash Resource but instead got "bogus"/
    end

    test "raises an error when the first argument is a module that is not an ash resource" do
      res = assert_raise RuntimeError, fn -> Api.read!(BadModuleName, []) end
      assert res.message =~ ~r/Ash.Test.AnyApi.read!\/2/

      assert res.message =~
               ~r/expected an %Ash.Query{} or an Ash Resource but instead got BadModuleName/
    end

    test "raises an error when the second argument is not a list" do
      res = assert_raise RuntimeError, fn -> Api.read!(Post, 1) end
      assert res.message =~ ~r/Ash.Test.AnyApi.read!\/2/
      assert res.message =~ ~r/expected a keyword list, but instead got 1/
    end

    test "raises an error when the second argument is not a valid keyword list" do
      res = assert_raise RuntimeError, fn -> Api.read!(Post, [1]) end
      assert res.message =~ ~r/Ash.Test.AnyApi.read!\/2/
      assert res.message =~ ~r/expected a keyword list, but instead got \[1\]/
    end

    test "raises an error when page is sent but pagination is not enabled on a resource" do
      res =
        assert_raise Ash.Error.Invalid, fn ->
          Api.read!(Post, page: [limit: 10])
        end

      assert %Ash.Error.Invalid.PageRequiresPagination{resource: Post, action: _} = hd(res.errors)
    end
  end

  describe "Api.read/2" do
    setup do
      post1 =
        Post
        |> new(%{title: "test", contents: "yeet"})
        |> Api.create!()

      post2 =
        Post
        |> new(%{title: "test1", contents: "yeet2"})
        |> Api.create!()

      %{post1: post1, post2: post2}
    end

    test "with a limit of 1, returns only 1 record" do
      assert {:ok, [_post]} =
               Post
               |> Ash.Query.limit(1)
               |> Api.read()
    end

    test "after action hooks are run" do
      assert [%{__metadata__: %{prepared?: true}}, %{__metadata__: %{prepared?: true}}] =
               Api.read!(Post, action: :read_with_after_action)
    end

    test "with a limit size of 2, returns 2 records" do
      assert {:ok, [_, _]} =
               Post
               |> Ash.Query.limit(2)
               |> Api.read()
    end

    test "with a limit of 1 and an offset of 1, it returns 1 record" do
      assert {:ok, [_]} =
               Post
               |> Ash.Query.limit(1)
               |> Ash.Query.offset(1)
               |> Api.read()
    end
  end

  describe "Api.read!/2" do
    setup do
      post1 =
        Post
        |> new(%{title: "test", contents: "yeet"})
        |> Api.create!()

      post2 =
        Post
        |> new(%{title: "test1", contents: "yeet2"})
        |> Api.create!()

      %{post1: post1, post2: post2}
    end

    test "it returns the records not in a tuple" do
      assert [_, _] = Api.read!(Post)
    end
  end

  describe "Api.read_one/2" do
    test "raises an error when the first argument is not a module" do
      res = assert_raise RuntimeError, fn -> Api.read_one("bogus", []) end
      assert res.message =~ ~r/Ash.Test.AnyApi.read_one\/2/

      assert res.message =~
               ~r/expected an %Ash.Query{} or an Ash Resource but instead got "bogus"/
    end

    test "raises an error when the first argument is a module that is not an ash resource" do
      res = assert_raise RuntimeError, fn -> Api.read_one(BadModuleName, []) end
      assert res.message =~ ~r/Ash.Test.AnyApi.read_one\/2/

      assert res.message =~
               ~r/expected an %Ash.Query{} or an Ash Resource but instead got BadModuleName/
    end

    test "raises an error when the second argument is not a list" do
      res = assert_raise RuntimeError, fn -> Api.read_one(Post, 1) end
      assert res.message =~ ~r/Ash.Test.AnyApi.read_one\/2/
      assert res.message =~ ~r/expected a keyword list, but instead got 1/
    end

    test "raises an error when the second argument is not a valid keyword list" do
      res = assert_raise RuntimeError, fn -> Api.read_one(Post, [1]) end
      assert res.message =~ ~r/Ash.Test.AnyApi.read_one\/2/
      assert res.message =~ ~r/expected a keyword list, but instead got \[1\]/
    end
  end

  describe "Api.read_one!/2" do
    test "raises an error when the first argument is not a module" do
      res = assert_raise RuntimeError, fn -> Api.read_one!("bogus", []) end
      assert res.message =~ ~r/Ash.Test.AnyApi.read_one!\/2/

      assert res.message =~
               ~r/expected an %Ash.Query{} or an Ash Resource but instead got "bogus"/
    end

    test "raises an error when the first argument is a module that is not an ash resource" do
      res = assert_raise RuntimeError, fn -> Api.read_one!(BadModuleName, []) end
      assert res.message =~ ~r/Ash.Test.AnyApi.read_one!\/2/

      assert res.message =~
               ~r/expected an %Ash.Query{} or an Ash Resource but instead got BadModuleName/
    end

    test "raises an error when the second argument is not a list" do
      res = assert_raise RuntimeError, fn -> Api.read_one!(Post, 1) end
      assert res.message =~ ~r/Ash.Test.AnyApi.read_one!\/2/
      assert res.message =~ ~r/expected a keyword list, but instead got 1/
    end

    test "raises an error when the second argument is not a valid keyword list" do
      res = assert_raise RuntimeError, fn -> Api.read_one!(Post, [1]) end
      assert res.message =~ ~r/Ash.Test.AnyApi.read_one!\/2/
      assert res.message =~ ~r/expected a keyword list, but instead got \[1\]/
    end
  end

  describe "filters" do
    setup do
      post1 =
        Post
        |> new(%{title: "test", contents: "yeet"})
        |> Api.create!()
        |> strip_metadata()

      post2 =
        Post
        |> new(%{title: "test1", contents: "yeet"})
        |> Api.create!()
        |> strip_metadata()

      %{post1: post1, post2: post2}
    end

    test "a filter that matches nothing returns no results" do
      assert {:ok, []} =
               Post
               |> Ash.Query.filter(contents == "not_yeet")
               |> Api.read()
    end

    test "a filter returns only matching records", %{post1: post1} do
      assert {:ok, [^post1]} =
               Post
               |> Ash.Query.filter(title == ^post1.title)
               |> Api.read()
               |> strip_metadata()
    end

    test "a filter returns multiple records if they match", %{post1: post1, post2: post2} do
      assert {:ok, [_, _] = results} =
               Post
               |> Ash.Query.filter(contents == "yeet")
               |> Api.read()
               |> strip_metadata()

      assert post1 in results
      assert post2 in results
    end
  end

  describe "select" do
    test "it automatically selects all fields" do
      author =
        Author
        |> new(%{name: "bruh"})
        |> Api.create!()

      assert author.name
      assert author.id
    end

    test "you can deselect a field" do
      Author
      |> new(%{name: "bruh"})
      |> Api.create!()

      assert [%{name: "bruh"}] = Api.read!(Author)
      assert [%{name: nil}] = Api.read!(Ash.Query.deselect(Author, :name))
    end

    test "you can select fields, but the primary key is always present" do
      Author
      |> new(%{name: "bruh"})
      |> Api.create!()

      assert [%{name: "bruh", id: id}] = Api.read!(Ash.Query.select(Author, :name))
      assert id
    end
  end

  describe "relationship filters" do
    setup do
      author1 =
        Author
        |> new(%{name: "bruh"})
        |> Api.create!()

      author2 =
        Author
        |> new(%{name: "bruh"})
        |> Api.create!()

      post =
        Post
        |> new(%{title: "test", contents: "yeet"})
        |> manage_relationship(:author1, author1, type: :append_and_remove)
        |> manage_relationship(:author2, author2, type: :append_and_remove)
        |> Api.create!()

      %{post: post, author1: author1, author2: author2}
    end

    test "you can filter on a related value", %{author1: author1} do
      assert [_] =
               Post
               |> Ash.Query.filter(author1: author1.id)
               |> Api.read!()
    end

    test "you can filter on multiple related values", %{author1: author1, author2: author2} do
      assert [_] =
               Post
               |> Ash.Query.filter(author1: author1.id, author2: author2.id)
               |> Api.read!()
    end
  end

  describe "sort" do
    setup do
      post1 =
        Post
        |> new(%{title: "abc", contents: "abc"})
        |> Api.create!()
        |> strip_metadata()

      post2 =
        Post
        |> new(%{title: "xyz", contents: "abc"})
        |> Api.create!()
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
               |> Api.read()
               |> strip_metadata()
    end

    test "a sort will sor rows accordingly when descending", %{
      post1: post1,
      post2: post2
    } do
      assert {:ok, [^post2, ^post1]} =
               Post
               |> Ash.Query.sort(title: :desc)
               |> Api.read()
               |> strip_metadata()
    end

    test "a nested sort sorts accordingly", %{post1: post1, post2: post2} do
      middle_post =
        Post
        |> new(%{title: "abc", contents: "xyz"})
        |> Api.create!()
        |> strip_metadata()

      assert {:ok, [^post1, ^middle_post, ^post2]} =
               Post
               |> Ash.Query.sort(title: :asc, contents: :asc)
               |> Api.read()
               |> strip_metadata()
    end

    test "a sort can use an expression", %{post1: post1, post2: post2} do
      require Ash.Sort

      Post
      |> Ash.Query.sort([{Ash.Sort.expr_sort(title <> contents), :asc}])
      |> Api.read!()
      |> IO.inspect()
    end
  end

  describe "get_by with only a single field" do
    setup do
      post =
        Enum.map(0..2, fn _ ->
          Post
          |> new(%{title: "test", contents: "yeet"})
          |> Api.create!()
          |> strip_metadata()
        end)
        |> Enum.random()

      %{post_id: post.id}
    end

    test "it succeeds when the record exists", %{post_id: post_id} do
      assert {:ok, %{id: ^post_id}} =
               Post |> Ash.Query.for_read(:get_by_id, %{id: post_id}) |> Api.read_one()
    end

    test "it fails when the record does not exist" do
      assert {:ok, nil} =
               Post
               |> Ash.Query.for_read(:get_by_id, %{id: Ash.UUID.generate()})
               |> Api.read_one()
    end
  end

  describe "get_by with multiple fields" do
    setup do
      post =
        Enum.map(0..2, fn _ ->
          Post
          |> new(%{title: "test", contents: "yeet"})
          |> Api.create!()
          |> strip_metadata()
        end)
        |> Enum.random()

      %{post_id: post.id, post_uuid: post.uuid}
    end

    test "it succeeds when the record exists", %{post_id: post_id, post_uuid: post_uuid} do
      assert {:ok, %{id: ^post_id, uuid: ^post_uuid}} =
               Post
               |> Ash.Query.for_read(:get_by_id_and_uuid, %{id: post_id, uuid: post_uuid})
               |> Api.read_one()
    end

    test "it fails when the record does not exist" do
      assert {:ok, nil} =
               Post
               |> Ash.Query.for_read(:get_by_id_and_uuid, %{
                 id: Ash.UUID.generate(),
                 uuid: Ash.UUID.generate()
               })
               |> Api.read_one()
    end
  end
end
