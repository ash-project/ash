# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Filter.ParentTest do
  use ExUnit.Case, async: false

  require Ash.Query
  alias Ash.Test.Domain, as: Domain

  defmodule User do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute(:name, :string, public?: true)
      attribute(:native_languages, {:array, :string}, public?: true)
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
      defaults [:read, :destroy, create: :*, update: :*]
    end

    attributes do
      uuid_primary_key :id
      attribute(:title, :string, public?: true)
      attribute(:contents, :string, public?: true)
      attribute(:points, :integer, public?: true)
      attribute(:language, :string, public?: true)
    end

    relationships do
      belongs_to(:author, User,
        public?: true,
        destination_attribute: :id,
        source_attribute: :author_id
      )

      has_many(:comments, Ash.Test.Filter.ParentTest.Comment,
        destination_attribute: :post_id,
        public?: true
      )
    end
  end

  defmodule Comment do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    attributes do
      uuid_primary_key :id
      attribute(:contents, :string, public?: true)
      attribute(:language, :string, public?: true)
    end

    relationships do
      belongs_to(:post, Post,
        public?: true,
        destination_attribute: :id,
        source_attribute: :post_id
      )

      has_one :post_author, User do
        no_attributes? true
        filter expr(id == parent(post.author_id))
      end
    end
  end

  defmodule ScopedNote do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Simple

    actions do
      defaults [:read]
    end

    attributes do
      attribute(:id, :integer, primary_key?: true, allow_nil?: false, public?: true)
      attribute(:org_id, :integer, public?: true)
      attribute(:secret, :string, public?: true)
    end
  end

  defmodule ScopeParent do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Simple

    actions do
      defaults [:read]
    end

    attributes do
      attribute(:id, :integer, primary_key?: true, allow_nil?: false, public?: true)
      attribute(:org_id, :integer, public?: true)
    end

    relationships do
      has_many :notes, ScopedNote do
        no_attributes? true
        filter expr(is_nil(parent(org_id)) or org_id == parent(org_id))
        public? true
      end
    end
  end

  defmodule Shelved do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      default_accept :*
      defaults [:destroy, create: :*, update: :*]

      read :read do
        primary? true
        filter expr(shelved == false)
      end
    end

    attributes do
      uuid_primary_key :id
      attribute(:kind, :string, public?: true)
      attribute(:shelved, :boolean, default: false, public?: true)
    end

    relationships do
      has_many :items, Ash.Test.Filter.ParentTest.ShelvedItem do
        public? true
        filter expr(kind == parent(kind))
      end
    end
  end

  defmodule ShelvedItem do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    attributes do
      uuid_primary_key :id
      attribute(:kind, :string, public?: true)
    end

    relationships do
      belongs_to :shelved, Ash.Test.Filter.ParentTest.Shelved do
        public? true
        attribute_writable? true
      end
    end
  end

  test "exists/2 can use `parent` to refer to the root record" do
    author =
      User
      |> Ash.Changeset.for_create(:create, %{name: "best"})
      |> Ash.create!()

    Post
    |> Ash.Changeset.for_create(:create, %{title: "best"})
    |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
    |> Ash.create!()

    assert [_] =
             User
             |> Ash.Query.filter(exists(posts, title == parent(name)))
             |> Ash.read!()

    assert [] =
             User
             |> Ash.Query.filter(exists(posts, title == parent(name <> "foo")))
             |> Ash.read!()
  end

  test "in/2 can use `parent` to refer to the root record" do
    author =
      User
      |> Ash.Changeset.for_create(:create, %{native_languages: ["en", "ko"]})
      |> Ash.create!()

    Post
    |> Ash.Changeset.for_create(:create, %{language: "en"})
    |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
    |> Ash.create!()

    assert [_] =
             User
             |> Ash.Query.filter(exists(posts, language in parent(native_languages)))
             |> Ash.read!()
  end

  test "parent can refer belongs_to relationship in query filter" do
    author =
      User
      |> Ash.Changeset.for_create(:create, %{name: "best", native_languages: ["english"]})
      |> Ash.create!()

    post =
      Post
      |> Ash.Changeset.for_create(:create, %{title: "best", language: "english"})
      |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
      |> Ash.create!()

    Comment
    |> Ash.Changeset.for_create(:create, %{contents: "best", language: "english"})
    |> Ash.Changeset.manage_relationship(:post, post, type: :append_and_remove)
    |> Ash.create!()

    assert [_] =
             Post
             |> Ash.Query.filter(exists(comments, language in parent(author.native_languages)))
             |> Ash.read!()
  end

  test "parent can refer belongs_to relationship in has_one filter" do
    author =
      User
      |> Ash.Changeset.for_create(:create, %{name: "best"})
      |> Ash.create!()

    post =
      Post
      |> Ash.Changeset.for_create(:create, %{title: "best"})
      |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
      |> Ash.create!()

    comment =
      Comment
      |> Ash.Changeset.for_create(:create, %{contents: "best"})
      |> Ash.Changeset.manage_relationship(:post, post, type: :append_and_remove)
      |> Ash.create!()
      |> Ash.load!(:post_author)

    assert comment.post_author.id == author.id
  end

  test "a failed parent() evaluation errors instead of activating the unrestricted branch" do
    data = %{
      ScopeParent => [struct(ScopeParent, id: 1, org_id: 10)],
      ScopedNote => [
        struct(ScopedNote, id: 1, org_id: 10, secret: "same organization"),
        struct(ScopedNote, id: 2, org_id: nil, secret: "unscoped private note")
      ]
    }

    base =
      ScopeParent
      |> Ash.Query.set_context(%{shared: %{data_layer: %{data: data}}})
      |> Ash.Query.filter(id == 1)
      |> Ash.Query.load(:notes)

    assert {:error, _} = base |> Ash.Query.select([:id]) |> Ash.read_one()

    assert {:ok, %{notes: notes}} = base |> Ash.Query.select([:id, :org_id]) |> Ash.read_one()
    assert Enum.map(notes, & &1.secret) == ["same organization"]
  end

  test "a relationship using `parent` loads on a record its source's read action no longer returns" do
    shelved = Ash.create!(Shelved, %{kind: "book"})
    item = Ash.create!(ShelvedItem, %{kind: "book", shelved_id: shelved.id})
    shelved = Ash.update!(shelved, %{shelved: true})

    assert [loaded] = Ash.load!(shelved, :items).items
    assert loaded.id == item.id
  end
end
