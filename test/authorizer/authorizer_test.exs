defmodule Ash.Test.Changeset.AuthorizerTest do
  @moduledoc false
  use ExUnit.Case, async: false

  require Ash.Query

  defmodule User do
    @moduledoc false

    use Ash.Resource,
      data_layer: Ash.DataLayer.Ets,
      domain: Ash.Test.Changeset.AuthorizerTest.Domain

    attributes do
      uuid_primary_key :id
    end

    actions do
      defaults [:read, :create]
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [
        Ash.Test.Authorizer,
        Ash.Policy.Authorizer
      ],
      domain: Ash.Test.Changeset.AuthorizerTest.Domain

    ets do
      private? true
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]

      create :title_is_authorization do
        accept []

        change fn changeset, context ->
          Ash.Changeset.change_attribute(changeset, :title, context.authorize?)
        end
      end

      update :update_title do
        accept [:title]
      end
    end

    relationships do
      belongs_to :user, User, public?: true
    end

    policies do
      policy action :update_title do
        authorize_if relates_to_actor_via(:user)
      end
    end

    attributes do
      uuid_primary_key :id

      attribute :title, :string, allow_nil?: false, public?: true
    end
  end

  defmodule Domain do
    @moduledoc false
    use Ash.Domain, otp_app: :ash

    resources do
      resource Post do
        define :update_post_title, action: :update_title
      end

      resource User
    end
  end

  describe "authorization options" do
    setup do
      on_exit(fn ->
        Application.delete_env(:ash, Domain)
      end)
    end

    test "authorize :always authorizes automatically" do
      Application.put_env(:ash, Domain,
        authorization: [
          authorize: :by_default
        ]
      )

      start_supervised({Ash.Test.Authorizer, strict_check: :forbidden})

      assert_raise Ash.Error.Forbidden, fn ->
        Post
        |> Ash.Changeset.for_create(:create, %{title: "test"})
        |> Ash.create!()
      end
    end

    test "authorize :by_default authorizes if actor is set" do
      Application.put_env(:ash, Domain,
        authorization: [
          authorize: :by_default
        ]
      )

      start_supervised({Ash.Test.Authorizer, strict_check: :authorized})

      post =
        Post
        |> Ash.Changeset.for_create(:title_is_authorization, %{}, actor: :an_actor)
        |> Ash.create!()

      assert post.title == "true"
    end

    test "require_actor? requires an actor for all requests" do
      Application.put_env(:ash, Domain,
        authorization: [
          require_actor?: true,
          authorize: :by_default
        ]
      )

      start_supervised({Ash.Test.Authorizer, strict_check: :forbidden})

      assert_raise Ash.Error.Forbidden, fn ->
        Post
        |> Ash.Changeset.for_create(:create, %{title: "test"})
        |> Ash.create!()
      end

      assert_raise Ash.Error.Forbidden, fn ->
        Post
        |> Ash.Changeset.for_create(:create, %{title: "test"})
        |> Ash.create!(actor: nil)
      end

      assert_raise Ash.Error.Forbidden, fn ->
        Post
        |> Ash.Changeset.for_create(:create, %{title: "test"}, actor: nil)
        |> Ash.create!()
      end
    end
  end

  describe "can domain checks" do
    test "a can check doesn't rerun queries" do
      start_supervised({Ash.Test.Authorizer, {}})

      user =
        User
        |> Ash.Changeset.for_create(:create)
        |> Ash.create!(authorize?: false)

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "test", user_id: user.id})
        |> Ash.create!(authorize?: false)
        |> Ash.load!(:user)

      # ! This causes an extra SQL query (!not when running against ETS)
      Domain.can_update_post_title(user, post)

      # In my actual usecase I have something like this in my liveview:
      # <div :for={item <- @items}>
      #   <button :if={MyDomain.can_delete(@current_user, item)}>Delete</button>
      #   {item.name}
      # </div>

      # This causes 1 query for every row.
    end
  end

  describe "strict check can filter results" do
    test "a simple filter is applied" do
      start_supervised(
        {Ash.Test.Authorizer,
         strict_check: {:filter, [title: "foo"]}, strict_check_context: [:query]}
      )

      Post
      |> Ash.Changeset.for_create(:create, %{title: "test"})
      |> Ash.create!(authorize?: false)

      Post
      |> Ash.Changeset.for_create(:create, %{title: "foo"})
      |> Ash.create!(authorize?: false)

      assert [%Post{title: "foo"}] = Ash.read!(Post, authorize?: true)
    end

    test "a filter cannot be applied to creates" do
      start_supervised(
        {Ash.Test.Authorizer,
         strict_check: {:filter, [title: "foo"]}, strict_check_context: [:query, :changeset]}
      )

      # Filter always fails on creates
      assert_raise Ash.Error.Forbidden, fn ->
        Post
        |> Ash.Changeset.for_create(:create, %{title: "test"}, authorize?: true)
        |> Ash.create!()
      end

      good_post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "foo"}, authorize?: false)
        |> Ash.create!()

      bad_post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "test"}, authorize?: false)
        |> Ash.create!()

      # Filters apply to the base data
      assert_raise Ash.Error.Forbidden, fn ->
        bad_post
        |> Ash.Changeset.for_update(:update, %{title: "next"}, authorize?: true)
        |> Ash.update!()
      end

      good_post
      |> Ash.Changeset.for_update(:update, %{title: "next"}, authorize?: true)
      |> Ash.update!()
    end
  end
end
