defmodule Ash.Test.Actions.HasManyTest do
  @moduledoc false
  use ExUnit.Case, async: true

  require Ash.Query

  defmodule Comment do
    use Ash.Resource,
      api: Ash.Test.Actions.HasManyTest.OtherApi,
      data_layer: Ash.DataLayer.Ets

    actions do
      defaults [:create, :read, :update, :destroy]
    end

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :post_id, :uuid
      attribute :content, :string
    end
  end

  defmodule OtherApi do
    use Ash.Api

    resources do
      resource Comment
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource,
      api: Ash.Test.Actions.HasManyTest.Api,
      data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      defaults [:create, :read, :update, :destroy]

      update :add_comment do
        accept []
        argument(:comment, :map, allow_nil?: false)
        change manage_relationship(:comment, :comments, on_no_match: :create, on_match: :update)
      end

      update :delete_comment do
        accept []
        argument(:comment, :map, allow_nil?: false)
        change manage_relationship(:comment, :comments, on_no_match: :error, on_match: :destroy)
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string
    end

    relationships do
      has_many :comments, Comment do
        destination_attribute :post_id
        api OtherApi
      end
    end
  end

  defmodule Api do
    @moduledoc false
    use Ash.Api

    resources do
      resource Post
    end
  end

  test "destroyed items are removed from the relationship" do
    post =
      Post
      |> Ash.Changeset.for_create(:create, %{
        title: "buz"
      })
      |> Api.create!()

    post =
      post
      |> Ash.Changeset.for_update(:add_comment, %{
        comment: %{content: "foo"}
      })
      |> Api.update!()

    assert length(post.comments) == 1

    post =
      post
      |> Ash.Changeset.for_update(:add_comment, %{
        comment: %{content: "bar"}
      })
      |> Api.update!()

    assert length(post.comments) == 2

    post =
      post
      |> Ash.Changeset.for_update(:delete_comment, %{
        comment: Enum.at(post.comments, 0)
      })
      |> Api.update!()

    assert length(post.comments) == 1
  end
end
