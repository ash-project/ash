defmodule Ash.Test.Resource.Changes.RelateActorTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule Author do
    use Ash.Resource,
      data_layer: Ash.DataLayer.Ets

    attributes do
      uuid_primary_key :id
    end

    actions do
      defaults [:create, :read]

      create :create_with_account do
        argument :account, :map
        change manage_relationship(:account, type: :append_and_remove)
      end
    end

    relationships do
      belongs_to :account, Ash.Test.Resource.Changes.RelateActorTest.Account
    end
  end

  defmodule Account do
    use Ash.Resource,
      data_layer: Ash.DataLayer.Ets

    attributes do
      uuid_primary_key :id
    end

    actions do
      defaults [:create, :read]
    end
  end

  defmodule Post do
    use Ash.Resource,
      data_layer: Ash.DataLayer.Ets

    attributes do
      uuid_primary_key :id
      attribute :text, :string
    end

    relationships do
      belongs_to :author, Ash.Test.Resource.Changes.RelateActorTest.Author do
        allow_nil? true
      end

      belongs_to :account, Ash.Test.Resource.Changes.RelateActorTest.Account do
        allow_nil? true
      end
    end

    actions do
      create :create_with_actor do
        change relate_actor(:author)
      end

      create :create_with_actor_field do
        change relate_actor(:account, field: :account)
      end

      create :create_possibly_without_actor do
        change relate_actor(:author, allow_nil?: true)
      end
    end
  end

  defmodule Registry do
    @moduledoc false
    use Ash.Registry

    entries do
      entry Account
      entry Author
      entry Post
    end
  end

  defmodule Api do
    use Ash.Api

    resources do
      registry Registry
    end
  end

  test "relate_actor change with defaults work" do
    actor =
      Author
      |> Ash.Changeset.for_create(:create)
      |> Api.create!()

    params = [text: "foo"]

    post_with =
      Post
      |> Ash.Changeset.for_create(:create_with_actor, params, actor: actor)
      |> Api.create!()

    assert post_with.author_id == actor.id

    {:error, changeset} =
      Post
      |> Ash.Changeset.for_create(:create_with_actor, params, actor: nil)
      |> Api.create()

    assert changeset.errors |> Enum.count() == 1
  end

  test "relate_actor change with field" do
    account =
      Account
      |> Ash.Changeset.for_create(:create, %{})
      |> Api.create!()

    actor =
      Author
      |> Ash.Changeset.for_create(:create_with_account, %{account: account})
      |> Api.create!()

    post =
      Post
      |> Ash.Changeset.for_create(:create_with_actor_field, %{text: "foo"}, actor: actor)
      |> Api.create!()

    assert post.account_id == account.id
  end

  test "relate_actor change with field when field is nil" do
    actor =
      Author
      |> Ash.Changeset.for_create(:create)
      |> Api.create!()
      |> Api.load!(:account)

    {:error, changeset} =
      Post
      |> Ash.Changeset.for_create(:create_with_actor_field, %{text: "foo"}, actor: actor)
      |> Api.create()

    assert changeset.errors |> Enum.count() == 1
  end

  test "relate_actor change with `allow_nil?: true` allows both nil and an actor" do
    actor =
      Author
      |> Ash.Changeset.for_create(:create)
      |> Api.create!()

    params = [text: "foo"]

    post_with =
      Post
      |> Ash.Changeset.for_create(:create_possibly_without_actor, params, actor: actor)
      |> Api.create!()

    assert post_with.author_id == actor.id

    post_without =
      Post
      |> Ash.Changeset.for_create(:create_possibly_without_actor, params, actor: nil)
      |> Api.create!()

    assert is_nil(post_without.author_id)
  end
end
