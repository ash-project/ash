defmodule Ash.Test.Filter.ParentTest do
  use ExUnit.Case, async: false

  import Ash.Changeset

  require Ash.Query

  defmodule User do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    actions do
      defaults [:create, :read, :update, :destroy]
    end

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute(:name, :string)
    end

    relationships do
      has_many(:posts, Ash.Test.Filter.ParentTest.Post, destination_attribute: :author_id)
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      defaults [:create, :read, :update, :destroy]
    end

    attributes do
      uuid_primary_key :id
      attribute(:title, :string)
      attribute(:contents, :string)
      attribute(:points, :integer)
    end

    relationships do
      belongs_to(:author, User,
        destination_attribute: :id,
        source_attribute: :author_id
      )
    end
  end

  defmodule Registry do
    @moduledoc false
    use Ash.Registry

    entries do
      entry(Post)
      entry(User)
    end
  end

  defmodule Api do
    @moduledoc false
    use Ash.Api

    resources do
      registry Registry
    end
  end

  test "exists/2 can use `parent` to refer to the root record" do
    author =
      User
      |> new(%{name: "best"})
      |> Api.create!()

    Post
    |> new(%{title: "best"})
    |> manage_relationship(:author, author, type: :append_and_remove)
    |> Api.create!()

    assert [_] =
             User
             |> Ash.Query.filter(exists(posts, title == parent(name)))
             |> Api.read!()

    assert [] =
             User
             |> Ash.Query.filter(exists(posts, title == parent(name <> "foo")))
             |> Api.read!()
  end
end
