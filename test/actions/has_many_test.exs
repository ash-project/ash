defmodule Ash.Test.Actions.HasManyTest do
  @moduledoc false
  use ExUnit.Case, async: true

  require Ash.Query

  defmodule Comment do
    use Ash.Resource,
      domain: Ash.Test.Actions.HasManyTest.OtherDomain,
      data_layer: Ash.DataLayer.Ets

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id

      attribute :post_id, :uuid do
        public?(true)
      end

      attribute :content, :string do
        public?(true)
      end
    end
  end

  defmodule OtherDomain do
    use Ash.Domain

    resources do
      resource Comment
    end
  end

  defmodule MeowCommentRelationship do
    use Ash.Resource.ManualRelationship

    require Ash.Query

    def load(records, _opts, %{query: query, actor: actor, authorize?: authorize?}) do
      post_ids = Enum.map(records, & &1.id)

      {:ok,
       query
       |> Ash.Query.filter(post_id in ^post_ids)
       |> Ash.Query.filter(content == "meow")
       |> Ash.read!(actor: actor, authorize?: authorize?)
       |> Enum.group_by(& &1.post_id)}
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
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]

      update :add_comment do
        require_atomic? false
        accept []
        argument(:comment, :map, allow_nil?: false)
        change manage_relationship(:comment, :comments, on_no_match: :create, on_match: :update)
      end

      update :delete_comment do
        require_atomic? false
        accept []
        argument(:comment, :map, allow_nil?: false)
        change manage_relationship(:comment, :comments, on_no_match: :error, on_match: :destroy)
      end
    end

    attributes do
      uuid_primary_key :id, type: :ci_string

      attribute :title, :string do
        public?(true)
      end
    end

    relationships do
      has_many :comments, Comment do
        destination_attribute :post_id
        public?(true)
        domain(OtherDomain)
      end

      has_many :meow_comments, Comment do
        manual MeowCommentRelationship
      end
    end
  end

  defmodule Domain do
    @moduledoc false
    use Ash.Domain

    resources do
      resource Post
      resource Comment
    end
  end

  test "destroyed items are removed from the relationship" do
    post =
      Post
      |> Ash.Changeset.for_create(:create, %{
        title: "buz"
      })
      |> Ash.create!()

    post =
      post
      |> Ash.Changeset.for_update(:add_comment, %{
        comment: %{content: "foo"}
      })
      |> Ash.update!()

    assert length(post.comments) == 1

    post =
      post
      |> Ash.Changeset.for_update(:add_comment, %{
        comment: %{content: "bar"}
      })
      |> Ash.update!()

    assert length(post.comments) == 2

    post =
      post
      |> Ash.Changeset.for_update(:delete_comment, %{
        comment: Enum.at(post.comments, 0)
      })
      |> Ash.update!()

    assert length(post.comments) == 1
  end

  test "manual relationships work as expected" do
    post =
      Post
      |> Ash.Changeset.for_create(:create, %{
        title: "buz"
      })
      |> Ash.create!()

    post =
      post
      |> Ash.Changeset.for_update(:add_comment, %{
        comment: %{content: "meow"}
      })
      |> Ash.update!()
      |> Ash.load!(:meow_comments)

    assert length(post.meow_comments) == 1

    post =
      post
      |> Ash.Changeset.for_update(:add_comment, %{
        comment: %{content: "bar"}
      })
      |> Ash.update!()

    post =
      post
      |> Ash.Changeset.for_update(:add_comment, %{
        comment: %{content: "meow"}
      })
      |> Ash.update!()
      |> Ash.load!(:meow_comments)

    assert length(post.meow_comments) == 2

    post =
      post
      |> Ash.Changeset.for_update(:delete_comment, %{
        comment: Enum.at(post.meow_comments, 0)
      })
      |> Ash.update!()
      |> Ash.load!(:meow_comments)

    assert length(post.meow_comments) == 1
  end
end
