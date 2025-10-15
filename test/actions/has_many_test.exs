# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

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

      attribute :title, :string, public?: true
      attribute :tenant_id, :string, public?: true

      create_timestamp :inserted_at, public?: true
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

  defmodule PostView do
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
    end

    attributes do
      uuid_primary_key :id

      attribute :last_post_id, :ci_string, public?: true
      attribute :user_id, :uuid, public?: true
    end

    relationships do
      belongs_to :user, Ash.Test.Actions.HasManyTest.User do
        source_attribute :user_id
        destination_attribute :id
        public?(true)
      end

      belongs_to :last_post, Post do
        source_attribute :last_post_id
        destination_attribute :id
        public?(true)
      end
    end
  end

  defmodule User do
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
    end

    attributes do
      uuid_primary_key :id

      attribute :name, :string, public?: true
      attribute :tenant_id, :string, public?: true
    end

    relationships do
      has_one :post_view, PostView do
        source_attribute :id
        destination_attribute :user_id
        public?(true)
      end

      has_many :unread_posts, Post do
        source_attribute :tenant_id
        destination_attribute :tenant_id
        public?(true)

        # This is evenn't
        filter expr(id != parent(post_view.last_post.id))
      end
    end
  end

  defmodule Domain do
    @moduledoc false
    use Ash.Domain

    resources do
      resource Post
      resource PostView
      resource Comment
      resource User
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

  test "raise on an invalid manual relationship query" do
    post =
      Post
      |> Ash.Changeset.for_create(:create, %{
        title: "buz"
      })
      |> Ash.create!()

    assert_raise Ash.Error.Invalid, ~r/Invalid query/, fn ->
      post
      |> Ash.load!(
        meow_comments:
          Comment
          |> Ash.Query.limit(1)
      )
    end
  end

  test "expr within relationship - 2" do
    tenant_id = Ash.UUID.generate()

    user =
      User
      |> Ash.Changeset.for_create(:create, %{name: "My Name", tenant_id: tenant_id})
      |> Ash.create!()

    _first_read_post =
      Post
      |> Ash.Changeset.for_create(:create, %{title: "Read Post 1", tenant_id: tenant_id})
      |> Ash.Changeset.force_change_attribute(
        :inserted_at,
        DateTime.utc_now() |> DateTime.add(-10, :second)
      )
      |> Ash.create!()

    last_read_post =
      Post
      |> Ash.Changeset.for_create(:create, %{title: "Read Post 2", tenant_id: tenant_id})
      |> Ash.Changeset.force_change_attribute(
        :inserted_at,
        DateTime.utc_now() |> DateTime.add(-9, :second)
      )
      |> Ash.create!()

    _unread_post =
      Post
      |> Ash.Changeset.for_create(:create, %{title: "Unread Post", tenant_id: tenant_id})
      |> Ash.Changeset.force_change_attribute(
        :inserted_at,
        DateTime.utc_now() |> DateTime.add(-8, :second)
      )
      |> Ash.create!()

    # Track which post was read last
    PostView
    |> Ash.Changeset.for_create(:create, %{last_post_id: last_read_post.id, user_id: user.id})
    |> Ash.create!()

    user = Ash.get!(User, to_string(user.id), load: :unread_posts, authorize?: false)
    assert length(user.unread_posts) == 2
  end
end
