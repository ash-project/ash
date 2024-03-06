defmodule Ash.Test.Filter.ParentTest do
  use ExUnit.Case, async: false

  require Ash.Query
  alias Ash.Test.Domain, as: Domain

  defmodule User do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    actions do
      default_accept :*
      defaults [:create, :read, :update, :destroy]
    end

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute(:name, :string, public?: true)
    end

    relationships do
      has_many(:posts, Ash.Test.Filter.ParentTest.Post,
        destination_attribute: :author_id,
        public?: true
      )
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      default_accept :*
      defaults [:create, :read, :update, :destroy]
    end

    attributes do
      uuid_primary_key :id
      attribute(:title, :string, public?: true)
      attribute(:contents, :string, public?: true)
      attribute(:points, :integer, public?: true)
    end

    relationships do
      belongs_to(:author, User,
        public?: true,
        destination_attribute: :id,
        source_attribute: :author_id
      )
    end
  end

  test "exists/2 can use `parent` to refer to the root record" do
    author =
      User
      |> Ash.Changeset.for_create(:create, %{name: "best"})
      |> Domain.create!()

    Post
    |> Ash.Changeset.for_create(:create, %{title: "best"})
    |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
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
