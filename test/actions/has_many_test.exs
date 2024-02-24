defmodule Ash.Test.Actions.HasManyTest do
  @moduledoc false
  use ExUnit.Case, async: true

  require Ash.Query

  defmodule Comment do
    use Ash.Resource,
      domain: Ash.Test.Actions.HasManyTest.OtherDomain,
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

  defmodule OtherDomain do
    use Ash.Domain

    resources do
      resource Comment
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource,
      domain: Ash.Test.Actions.HasManyTest.Domain,
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
        domain(OtherDomain)
      end
    end
  end

  defmodule Domain do
    @moduledoc false
    use Ash.Domain

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
      |> Domain.create!()

    post =
      post
      |> Ash.Changeset.for_update(:add_comment, %{
        comment: %{content: "foo"}
      })
      |> Domain.update!()

    assert length(post.comments) == 1

    post =
      post
      |> Ash.Changeset.for_update(:add_comment, %{
        comment: %{content: "bar"}
      })
      |> Domain.update!()

    assert length(post.comments) == 2

    post =
      post
      |> Ash.Changeset.for_update(:delete_comment, %{
        comment: Enum.at(post.comments, 0)
      })
      |> Domain.update!()

    assert length(post.comments) == 1
  end
end
