defmodule Ash.Test.Actions.CreateTest do
  use ExUnit.Case, async: true

  defmodule Author do
    use Ash.Resource, name: "authors", type: "author"
    use Ash.DataLayer.Ets, private?: true

    actions do
      read :default
      create :default
    end

    attributes do
      attribute :name, :string
    end
  end

  defmodule PostDefaults do
    def garbage2(), do: "garbage2"
    def garbage3(), do: "garbage3"
  end

  defmodule Post do
    use Ash.Resource, name: "posts", type: "post"
    use Ash.DataLayer.Ets, private?: true

    actions do
      read :default
      create :default
    end

    attributes do
      attribute :title, :string
      attribute :contents, :string
      attribute :tag, :string, default: {:constant, "garbage"}
      attribute :tag2, :string, default: &PostDefaults.garbage2/0
      attribute :tag3, :string, default: {PostDefaults, :garbage3}
    end

    relationships do
      belongs_to :author, Author
    end
  end

  defmodule Api do
    use Ash.Api

    resources [Author, Post]
  end

  describe "simple creates" do
    test "allows creating a record with valid attributes" do
      assert %Post{title: "foo", contents: "bar"} =
               Api.create!(Post, %{attributes: %{title: "foo", contents: "bar"}})
    end

    test "constant default values are set properly" do
      assert %Post{tag: "garbage"} = Api.create!(Post, %{attributes: %{title: "foo"}})
    end

    test "constant functions values are set properly" do
      assert %Post{tag2: "garbage2"} = Api.create!(Post, %{attributes: %{title: "foo"}})
    end

    test "constant module/function values are set properly" do
      assert %Post{tag3: "garbage3"} = Api.create!(Post, %{attributes: %{title: "foo"}})
    end
  end
end
