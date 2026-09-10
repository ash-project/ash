# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Policy.CanReadTest do
  @doc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule RuntimeCheck do
    @moduledoc false
    use Ash.Policy.Check

    def describe(_), do: "a runtime check"

    def strict_check(_, _, _), do: {:ok, :unknown}

    def check(_, items, _, _), do: items
  end

  defmodule User do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      default_accept :*
      defaults [:read, create: :*]
    end

    attributes do
      uuid_primary_key :id
      attribute :admin, :boolean, default: false, public?: true
    end
  end

  defmodule Organization do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private? true
    end

    actions do
      default_accept :*
      defaults [:read, create: :*]
    end

    attributes do
      uuid_primary_key :id
    end

    relationships do
      has_many :memberships, Ash.Test.Policy.CanReadTest.Membership, public?: true
    end

    policies do
      policy action_type(:read) do
        # to-many: readable if any membership is readable
        authorize_if can_read(:memberships)
      end
    end
  end

  defmodule Membership do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private? true
    end

    actions do
      default_accept :*
      defaults [:read, create: :*]
    end

    attributes do
      uuid_primary_key :id
    end

    relationships do
      belongs_to :organization, Ash.Test.Policy.CanReadTest.Organization, public?: true
      belongs_to :user, Ash.Test.Policy.CanReadTest.User, public?: true
    end

    policies do
      policy action_type(:read) do
        authorize_if relates_to_actor_via(:user)
      end
    end
  end

  defmodule Team do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private? true
    end

    actions do
      default_accept :*
      defaults [:read, create: :*]
    end

    attributes do
      uuid_primary_key :id
    end

    relationships do
      belongs_to :organization, Ash.Test.Policy.CanReadTest.Organization, public?: true
    end

    policies do
      policy action_type(:read) do
        # nested: Team -> Organization -> Membership -> actor
        authorize_if can_read(:organization)
      end
    end
  end

  defmodule Project do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private? true
    end

    actions do
      default_accept :*
      defaults [:read, create: :*]

      read :via_path do
        # multi-step relationship path
      end
    end

    attributes do
      uuid_primary_key :id
    end

    relationships do
      belongs_to :team, Ash.Test.Policy.CanReadTest.Team, public?: true
    end

    policies do
      policy action(:read) do
        authorize_if can_read(:team)
      end

      policy action(:via_path) do
        authorize_if can_read([:team, :organization])
      end
    end
  end

  defmodule Post do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private? true
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]

      read :published do
        filter expr(published == true)
      end

      read :admins_only

      read :runtime_checked

      read :with_arg do
        argument :required_arg, :string, allow_nil?: false
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :public, :boolean, default: false, public?: true
      attribute :published, :boolean, default: false, public?: true
    end

    relationships do
      belongs_to :author, Ash.Test.Policy.CanReadTest.User, public?: true
      has_many :comments, Ash.Test.Policy.CanReadTest.Comment, public?: true

      has_many :short_circuit_comments, Ash.Test.Policy.CanReadTest.Comment do
        public? true
        destination_attribute :post_id
        read_action :via_accessing_from
      end
    end

    policies do
      policy action([:read, :published, :with_arg]) do
        authorize_if expr(public == true)
        authorize_if relates_to_actor_via(:author)
      end

      policy action(:admins_only) do
        authorize_if actor_attribute_equals(:admin, true)
      end

      policy action(:runtime_checked) do
        access_type :runtime
        authorize_if RuntimeCheck
      end

      policy action_type([:create, :update, :destroy]) do
        authorize_if always()
      end
    end
  end

  defmodule Comment do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private? true
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]

      read :via_published
      read :via_admins_only
      read :via_runtime_checked
      read :via_with_arg
      read :via_missing_action
      read :via_missing_relationship
      read :via_accessing_from
    end

    attributes do
      uuid_primary_key :id
      attribute :text, :string, public?: true
    end

    relationships do
      belongs_to :post, Ash.Test.Policy.CanReadTest.Post, public?: true
    end

    policies do
      policy action(:read) do
        authorize_if can_read(:post)
      end

      policy action(:via_published) do
        authorize_if can_read(:post, action: :published)
      end

      policy action(:via_admins_only) do
        authorize_if can_read(:post, action: :admins_only)
      end

      policy action(:via_runtime_checked) do
        authorize_if can_read(:post, action: :runtime_checked)
      end

      policy action(:via_with_arg) do
        authorize_if can_read(:post, action: :with_arg)
      end

      policy action(:via_missing_action) do
        authorize_if can_read(:post, action: :does_not_exist)
      end

      policy action(:via_missing_relationship) do
        authorize_if can_read(:does_not_exist)
      end

      policy action(:via_accessing_from) do
        authorize_if accessing_from(Ash.Test.Policy.CanReadTest.Post, :short_circuit_comments)
        # would raise if evaluated, since `:runtime_checked` requires runtime checks
        authorize_if can_read(:post, action: :runtime_checked)
      end

      policy action_type(:create) do
        authorize_if always()
      end

      policy action_type([:update, :destroy]) do
        authorize_if can_read(:post)
      end
    end
  end

  defmodule CycleA do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private? true
    end

    actions do
      default_accept :*
      defaults [:read, create: :*]
    end

    attributes do
      uuid_primary_key :id
    end

    relationships do
      has_many :bs, Ash.Test.Policy.CanReadTest.CycleB,
        destination_attribute: :a_id,
        public?: true
    end

    policies do
      policy action_type(:read) do
        authorize_if can_read(:bs)
      end
    end
  end

  defmodule CycleB do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private? true
    end

    actions do
      default_accept :*
      defaults [:read, create: :*]
    end

    attributes do
      uuid_primary_key :id
    end

    relationships do
      belongs_to :a, Ash.Test.Policy.CanReadTest.CycleA, public?: true
    end

    policies do
      policy action_type(:read) do
        authorize_if can_read(:a)
      end
    end
  end

  setup do
    actor = Ash.create!(User, %{}, authorize?: false)
    other = Ash.create!(User, %{}, authorize?: false)
    admin = Ash.create!(User, %{admin: true}, authorize?: false)

    %{actor: actor, other: other, admin: admin}
  end

  defp ids(records), do: Enum.map(records, & &1.id)

  defp create_comment(post) do
    post_id = if post, do: post.id, else: nil
    Ash.create!(Comment, %{post_id: post_id, text: "comment"}, authorize?: false)
  end

  describe "to-one relationships" do
    test "passes when the related record is readable by the actor", %{
      actor: actor,
      other: other
    } do
      public_post = Ash.create!(Post, %{public: true, author_id: other.id}, authorize?: false)
      own_post = Ash.create!(Post, %{public: false, author_id: actor.id}, authorize?: false)
      hidden_post = Ash.create!(Post, %{public: false, author_id: other.id}, authorize?: false)

      readable_1 = create_comment(public_post)
      readable_2 = create_comment(own_post)
      unreadable = create_comment(hidden_post)
      orphan = create_comment(nil)

      result_ids = Comment |> Ash.read!(actor: actor) |> ids()

      assert readable_1.id in result_ids
      assert readable_2.id in result_ids
      refute unreadable.id in result_ids
      refute orphan.id in result_ids
    end

    test "does not pass when the relationship is empty", %{actor: actor} do
      orphan = create_comment(nil)

      refute orphan.id in (Comment |> Ash.read!(actor: actor) |> ids())
    end

    test "uses the related read action's filter when given an action", %{actor: actor} do
      published = Ash.create!(Post, %{public: true, published: true}, authorize?: false)
      unpublished = Ash.create!(Post, %{public: true, published: false}, authorize?: false)
      hidden_published = Ash.create!(Post, %{public: false, published: true}, authorize?: false)

      readable = create_comment(published)
      not_published = create_comment(unpublished)
      not_readable = create_comment(hidden_published)

      result_ids =
        Comment
        |> Ash.Query.for_read(:via_published, %{}, actor: actor)
        |> Ash.read!()
        |> ids()

      assert readable.id in result_ids
      refute not_published.id in result_ids
      refute not_readable.id in result_ids
    end

    test "returns nothing when the related action is statically forbidden for the actor", %{
      actor: actor,
      admin: admin
    } do
      post = Ash.create!(Post, %{public: true}, authorize?: false)
      comment = create_comment(post)
      orphan = create_comment(nil)

      assert [] =
               Comment
               |> Ash.Query.for_read(:via_admins_only, %{}, actor: actor)
               |> Ash.read!()

      admin_ids =
        Comment
        |> Ash.Query.for_read(:via_admins_only, %{}, actor: admin)
        |> Ash.read!()
        |> ids()

      assert comment.id in admin_ids
      # admins can read all posts, but there still needs to be a post
      refute orphan.id in admin_ids
    end

    test "works with Ash.can? for single records", %{actor: actor, other: other} do
      public_post = Ash.create!(Post, %{public: true, author_id: other.id}, authorize?: false)
      hidden_post = Ash.create!(Post, %{public: false, author_id: other.id}, authorize?: false)

      readable = create_comment(public_post)
      unreadable = create_comment(hidden_post)

      assert Ash.can?({Comment, :read}, actor, data: [readable])
      refute Ash.can?({Comment, :read}, actor, data: [unreadable])
    end
  end

  describe "to-many and nested relationships" do
    test "passes when any related record is readable", %{actor: actor, other: other} do
      my_org = Ash.create!(Organization, %{}, authorize?: false)
      other_org = Ash.create!(Organization, %{}, authorize?: false)
      empty_org = Ash.create!(Organization, %{}, authorize?: false)

      Ash.create!(Membership, %{organization_id: my_org.id, user_id: actor.id}, authorize?: false)

      Ash.create!(Membership, %{organization_id: my_org.id, user_id: other.id}, authorize?: false)

      Ash.create!(Membership, %{organization_id: other_org.id, user_id: other.id},
        authorize?: false
      )

      result_ids = Organization |> Ash.read!(actor: actor) |> ids()

      assert my_org.id in result_ids
      refute other_org.id in result_ids
      refute empty_org.id in result_ids
    end

    test "composes across multiple levels of can_read", %{actor: actor, other: other} do
      my_org = Ash.create!(Organization, %{}, authorize?: false)
      other_org = Ash.create!(Organization, %{}, authorize?: false)

      Ash.create!(Membership, %{organization_id: my_org.id, user_id: actor.id}, authorize?: false)

      Ash.create!(Membership, %{organization_id: other_org.id, user_id: other.id},
        authorize?: false
      )

      my_team = Ash.create!(Team, %{organization_id: my_org.id}, authorize?: false)
      other_team = Ash.create!(Team, %{organization_id: other_org.id}, authorize?: false)

      my_project = Ash.create!(Project, %{team_id: my_team.id}, authorize?: false)
      other_project = Ash.create!(Project, %{team_id: other_team.id}, authorize?: false)

      team_ids = Team |> Ash.read!(actor: actor) |> ids()
      assert my_team.id in team_ids
      refute other_team.id in team_ids

      project_ids = Project |> Ash.read!(actor: actor) |> ids()
      assert my_project.id in project_ids
      refute other_project.id in project_ids
    end

    test "supports multi-step relationship paths", %{actor: actor, other: other} do
      my_org = Ash.create!(Organization, %{}, authorize?: false)
      other_org = Ash.create!(Organization, %{}, authorize?: false)

      Ash.create!(Membership, %{organization_id: my_org.id, user_id: actor.id}, authorize?: false)

      Ash.create!(Membership, %{organization_id: other_org.id, user_id: other.id},
        authorize?: false
      )

      my_team = Ash.create!(Team, %{organization_id: my_org.id}, authorize?: false)
      other_team = Ash.create!(Team, %{organization_id: other_org.id}, authorize?: false)

      my_project = Ash.create!(Project, %{team_id: my_team.id}, authorize?: false)
      other_project = Ash.create!(Project, %{team_id: other_team.id}, authorize?: false)

      project_ids =
        Project
        |> Ash.Query.for_read(:via_path, %{}, actor: actor)
        |> Ash.read!()
        |> ids()

      assert my_project.id in project_ids
      refute other_project.id in project_ids
    end
  end

  describe "short-circuiting with accessing_from" do
    test "does not authorize the related action when accessing through the relationship", %{
      actor: actor
    } do
      post = Ash.create!(Post, %{public: true}, authorize?: false)
      comment = create_comment(post)

      # direct reads evaluate `can_read` and hit the runtime check error
      assert_raise Ash.Error.Unknown, ~r/require runtime checks/, fn ->
        Comment
        |> Ash.Query.for_read(:via_accessing_from, %{}, actor: actor)
        |> Ash.read!()
      end

      # loading through the relationship short-circuits before `can_read` is evaluated
      assert [%Comment{id: comment_id}] =
               post
               |> Ash.load!(:short_circuit_comments, actor: actor, authorize?: true)
               |> Map.get(:short_circuit_comments)

      assert comment_id == comment.id
    end
  end

  describe "non-read actions" do
    test "applies to the original data for updates and destroys", %{actor: actor, other: other} do
      public_post = Ash.create!(Post, %{public: true, author_id: other.id}, authorize?: false)
      hidden_post = Ash.create!(Post, %{public: false, author_id: other.id}, authorize?: false)

      readable = create_comment(public_post)
      unreadable = create_comment(hidden_post)

      assert %Comment{text: "updated"} =
               readable
               |> Ash.Changeset.for_update(:update, %{text: "updated"}, actor: actor)
               |> Ash.update!()

      assert {:error, %Ash.Error.Forbidden{}} =
               unreadable
               |> Ash.Changeset.for_update(:update, %{text: "updated"}, actor: actor)
               |> Ash.update()

      assert :ok =
               readable
               |> Ash.Changeset.for_destroy(:destroy, %{}, actor: actor)
               |> Ash.destroy!()

      assert {:error, %Ash.Error.Forbidden{}} =
               unreadable
               |> Ash.Changeset.for_destroy(:destroy, %{}, actor: actor)
               |> Ash.destroy()
    end
  end

  describe "errors" do
    test "raises when the related action requires runtime checks", %{actor: actor} do
      assert_raise Ash.Error.Unknown, ~r/require runtime checks/, fn ->
        Comment
        |> Ash.Query.for_read(:via_runtime_checked, %{}, actor: actor)
        |> Ash.read!()
      end
    end

    test "raises when the related action has required arguments", %{actor: actor} do
      assert_raise Ash.Error.Unknown, ~r/required arguments/, fn ->
        Comment
        |> Ash.Query.for_read(:via_with_arg, %{}, actor: actor)
        |> Ash.read!()
      end
    end

    test "raises when the related action does not exist", %{actor: actor} do
      assert_raise Ash.Error.Unknown, ~r/No such read action `:does_not_exist`/, fn ->
        Comment
        |> Ash.Query.for_read(:via_missing_action, %{}, actor: actor)
        |> Ash.read!()
      end
    end

    test "raises when the relationship does not exist", %{actor: actor} do
      assert_raise Ash.Error.Unknown, ~r/No such relationship `:does_not_exist`/, fn ->
        Comment
        |> Ash.Query.for_read(:via_missing_relationship, %{}, actor: actor)
        |> Ash.read!()
      end
    end

    test "raises on cycles between can_read checks", %{actor: actor} do
      assert_raise Ash.Error.Unknown, ~r/Detected a cycle in `can_read` checks/, fn ->
        Ash.read!(CycleA, actor: actor)
      end
    end
  end

  test "describe/1" do
    assert Ash.Policy.Check.CanRead.describe(relationship_path: [:post], action: nil) ==
             "actor can read record.post"

    assert Ash.Policy.Check.CanRead.describe(
             relationship_path: [:team, :organization],
             action: :visible
           ) ==
             "actor can read record.team.organization via :visible"
  end
end
