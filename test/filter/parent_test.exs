defmodule Ash.Test.Filter.ParentTest do
  use ExUnit.Case, async: false

  require Ash.Query
  alias Ash.Test.Domain, as: Domain

  defmodule User do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

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
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

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

  import Ash.Changeset

  test "exists/2 can use `parent` to refer to the root record" do
    author =
      User
      |> new(%{name: "best"})
      |> Domain.create!()

    Post
    |> new(%{title: "best"})
    |> manage_relationship(:author, author, type: :append_and_remove)
    |> Domain.create!()

    assert [_] =
             User
             |> Ash.Query.filter(exists(posts, title == parent(name)))
             |> Domain.read!()

    assert [] =
             User
             |> Ash.Query.filter(exists(posts, title == parent(name <> "foo")))
             |> Domain.read!()
  end
end
