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
      create :create
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
      belongs_to :author, Author do
        required? false
      end
    end

    actions do
      create :create_with_actor do
        change relate_actor(:author)
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
