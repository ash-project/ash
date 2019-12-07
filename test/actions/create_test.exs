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
  end
end
