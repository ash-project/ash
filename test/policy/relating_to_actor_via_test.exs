defmodule Ash.Test.Policy.RelatingToActorViaTest do
  @doc false
  use ExUnit.Case

  alias Ash.Test.Domain, as: Domain

  defmodule User do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    actions do
      default_accept(:*)
      defaults([:read, :destroy, create: :*, update: :*])
    end

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key(:id)

      attribute(:c, :string, allow_nil?: false, public?: true)
      attribute(:d, :string, allow_nil?: false, public?: true)
    end
  end

  defmodule Maintainer do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    actions do
      default_accept(:*)
      defaults([:read, :destroy, create: :*, update: :*])
    end

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key(:id)

      attribute(:post_id, :uuid)

      attribute(:a, :string, allow_nil?: false, public?: true)
      attribute(:b, :string, allow_nil?: false, public?: true)
    end

    relationships do
      belongs_to :user, Ash.Test.Policy.RelatingToActorViaTest.User do
        public?(true)
      end

      belongs_to :post, Ash.Test.Policy.RelatingToActorViaTest.Post do
        public?(true)
      end
    end
  end

  defmodule Post do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    actions do
      default_accept(:*)
      defaults([:read, :destroy, create: :*, update: :*])
    end

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key(:id)
    end

    relationships do
      belongs_to :user, Ash.Test.Policy.RelatingToActorViaTest.User do
        public?(true)
      end

      has_many :comments, Ash.Test.Policy.RelatingToActorViaTest.Comment do
        public?(true)
      end

      has_one :maintainer, Ash.Test.Policy.RelatingToActorViaTest.Maintainer do
        public?(true)
      end
    end
  end

  defmodule Comment do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets

    actions do
      default_accept(:*)
      defaults([:read, :destroy, create: :*, update: :*])
    end

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key(:id)
    end

    relationships do
      belongs_to :post, Ash.Test.Policy.RelatingToActorViaTest.Post do
        public?(true)
      end
    end
  end

  defmodule CommentMetadata do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    actions do
      default_accept(:*)
      defaults([:read, :destroy, create: :*, update: :*])
    end

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key(:id)
    end

    relationships do
      belongs_to :comment, Ash.Test.Policy.RelatingToActorViaTest.Comment do
        public?(true)
      end
    end

    policies do
      policy action_type(:create) do
        authorize_if(relating_to_actor_via([:comment, :post], target_field: :user_id))
      end
    end
  end

  defmodule Reaction do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    actions do
      default_accept(:*)
      defaults([:read, :destroy, create: :*, update: :*])
    end

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key(:id)
    end

    relationships do
      belongs_to :comment, Ash.Test.Policy.RelatingToActorViaTest.Comment do
        public?(true)
      end
    end

    policies do
      policy action_type(:create) do
        authorize_if(relating_to_actor_via([:comment, :post, :user]))
      end
    end
  end

  defmodule OtherThing do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    actions do
      default_accept(:*)
      defaults([:read, :destroy, create: :*, update: :*])
    end

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key(:id)
    end

    relationships do
      belongs_to :comment, Ash.Test.Policy.RelatingToActorViaTest.Comment do
        public?(true)
      end
    end

    policies do
      policy action_type(:create) do
        authorize_if(
          relating_to_actor_via([:comment, :post, :maintainer],
            target_field: [:a, :b],
            field: [:c, :d]
          )
        )
      end
    end
  end

  defmodule BadPolicyToMany do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    actions do
      default_accept(:*)
      defaults([:read, :destroy, create: :*, update: :*])
    end

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key(:id)
    end

    relationships do
      belongs_to(:post, Ash.Test.Policy.RelatingToActorViaTest.Post)
    end

    policies do
      policy action_type(:create) do
        authorize_if(relating_to_actor_via([:post, :comments, :post, :user]))
      end
    end
  end

  defmodule BadPolicyRelName do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    actions do
      default_accept(:*)
      defaults([:read, :destroy, create: :*, update: :*])
    end

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key(:id)
    end

    policies do
      policy always() do
        authorize_if(relating_to_actor_via(:does_not_exist))
      end
    end
  end

  defmodule BadPolicyRelPathName do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    actions do
      default_accept(:*)
      defaults([:read, :destroy, create: :*, update: :*])
    end

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key(:id)
    end

    policies do
      policy always() do
        authorize_if(relating_to_actor_via([:does_not_exist, :neither_does_this]))
      end
    end
  end

  defmodule BadPolicyTargetField do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    actions do
      default_accept(:*)
      defaults([:read, :destroy, create: :*, update: :*])
    end

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key(:id)
    end

    relationships do
      belongs_to(:comment, Ash.Test.Policy.RelatingToActorViaTest.Comment) do
        public?(true)
      end
    end

    policies do
      policy always() do
        authorize_if(
          relating_to_actor_via([:comment, :post, :user], target_field: :does_not_exist)
        )
      end
    end
  end

  defp user_fixture do
    User
    |> Ash.Changeset.for_create(:create, %{c: "key1", d: "key2"})
    |> Ash.create!()
  end

  setup do
    user = user_fixture()

    post =
      Post
      |> Ash.Changeset.for_create(:create)
      |> Ash.Changeset.manage_relationship(:user, user, type: :append)
      |> Ash.create!(authorize?: false)

    maintainer =
      Maintainer
      |> Ash.Changeset.for_create(:create, %{a: "key1", b: "key2"})
      |> Ash.Changeset.manage_relationship(:post, post, type: :append)
      |> Ash.create!(authorize?: false)

    comment =
      Comment
      |> Ash.Changeset.for_create(:create)
      |> Ash.Changeset.manage_relationship(:post, post, type: :append)
      |> Ash.create!(authorize?: false)

    %{comment: comment, maintainer: maintainer, post: post, user: user}
  end

  describe "no options" do
    test "relating_to_actor_via with create attribute", %{comment: comment, user: user} do
      assert {:ok, _} =
               Reaction
               |> Ash.Changeset.for_create(:create, %{comment_id: comment.id})
               |> Ash.create(actor: user)
    end

    test "relating_to_actor_via errors with other actor", %{comment: comment} do
      user = user_fixture()

      assert {:error, %Ash.Error.Forbidden{}} =
               Reaction
               |> Ash.Changeset.for_create(:create, %{comment_id: comment.id})
               |> Ash.create(actor: user)
    end

    # TODO: how can we make this work?
    @tag :skip
    test "relating_to_actor_via with manage_relationship", %{comment: comment, user: user} do
      assert {:ok, _} =
               Reaction
               |> Ash.Changeset.for_create(:create)
               |> Ash.Changeset.manage_relationship(:comment, comment, type: :append)
               |> Ash.create(actor: user)
    end

    test "relating_to_actor_via with to_many raises", %{user: user} do
      assert_raise Ash.Error.Unknown, ~r/`belongs_to` and `has_one` relationships/, fn ->
        BadPolicyToMany
        |> Ash.Changeset.for_create(:create)
        |> Ash.create!(authorize?: true, actor: user)
      end
    end

    test "relating_to_actor_via raises if relationship does not exist", %{user: user} do
      assert_raise Ash.Error.Unknown, ~r/does_not_exist/, fn ->
        BadPolicyRelName
        |> Ash.Changeset.for_create(:create)
        |> Ash.create!(authorize?: true, actor: user)
      end

      assert_raise Ash.Error.Unknown, ~r/:does_not_exist/, fn ->
        BadPolicyRelPathName
        |> Ash.Changeset.for_create(:create)
        |> Ash.create!(authorize?: true, actor: user)
      end
    end
  end

  describe "target_field option" do
    test "relating_to_actor_via with target_field", %{comment: comment, user: user} do
      assert {:ok, _} =
               CommentMetadata
               |> Ash.Changeset.for_create(:create, %{comment_id: comment.id})
               |> Ash.create(authorize?: true, actor: user)
    end

    test "relating_to_actor_via with multi-field target_field and field", %{
      comment: comment,
      user: user
    } do
      assert {:ok, _} =
               OtherThing
               |> Ash.Changeset.for_create(:create, %{comment_id: comment.id})
               |> Ash.create(authorize?: true, actor: user)
    end

    test "relating_to_actor_via raises with invalid target_field", %{comment: comment, user: user} do
      assert_raise Ash.Error.Unknown, ~r/does_not_exist/, fn ->
        BadPolicyTargetField
        |> Ash.Changeset.for_create(:create, %{comment_id: comment.id})
        |> Ash.create!(authorize?: true, actor: user)
      end
    end
  end
end
