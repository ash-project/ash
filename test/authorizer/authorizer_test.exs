defmodule Ash.Test.Changeset.AuthorizerTest do
  @moduledoc false
  use ExUnit.Case, async: false

  require Ash.Query

  defmodule Post do
    use Ash.Resource,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [
        Ash.Test.Authorizer
      ]

    ets do
      private? true
    end

    actions do
      defaults [:create, :read, :update, :destroy]

      create :title_is_authorization do
        accept []

        change fn changeset, context ->
          IO.inspect(context)
          Ash.Changeset.change_attribute(changeset, :title, context.authorize?)
        end
      end
    end

    attributes do
      uuid_primary_key :id

      attribute :title, :string, allow_nil?: false
    end
  end

  defmodule Registry do
    @moduledoc false
    use Ash.Registry

    entries do
      entry Post
    end
  end

  defmodule Api do
    use Ash.Api, otp_app: :ash

    resources do
      registry Registry
    end
  end

  describe "authorization options" do
    setup do
      on_exit(fn ->
        Application.delete_env(:ash, Api)
      end)
    end

    test "authorize :always authorizes automatically" do
      Application.put_env(:ash, Api,
        authorization: [
          authorize: :by_default
        ]
      )

      start_supervised({Ash.Test.Authorizer, strict_check: :forbidden})

      assert_raise Ash.Error.Forbidden, fn ->
        Post
        |> Ash.Changeset.for_create(:create, %{title: "test"})
        |> Api.create!()
      end
    end

    # TODO: this needs to be addressed in ash 3.0
    # test "authorize :by_default authorizes if actor is set" do
    #   Application.put_env(:ash, Api,
    #     authorization: [
    #       authorize: :by_default
    #     ]
    #   )

    #   start_supervised({Ash.Test.Authorizer, strict_check: :authorized})

    #   post =
    #     Post
    #     |> Ash.Changeset.for_create(:title_is_authorization, %{}, actor: :an_actor)
    #     |> Api.create!()

    #   assert post.title == "true"
    # end

    test "require_actor? requires an actor for all requests" do
      Application.put_env(:ash, Api,
        authorization: [
          require_actor?: true,
          authorize: :by_default
        ]
      )

      start_supervised({Ash.Test.Authorizer, strict_check: :forbidden})

      assert_raise Ash.Error.Forbidden, fn ->
        Post
        |> Ash.Changeset.for_create(:create, %{title: "test"})
        |> Api.create!()
      end

      assert_raise Ash.Error.Forbidden, fn ->
        Post
        |> Ash.Changeset.for_create(:create, %{title: "test"})
        |> Api.create!(actor: nil)
      end

      assert_raise Ash.Error.Forbidden, fn ->
        Ash.set_actor(nil)

        Post
        |> Ash.Changeset.for_create(:create, %{title: "test"})
        |> Api.create!()
      end
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
      |> Api.create!()

      Post
      |> Ash.Changeset.for_create(:create, %{title: "foo"})
      |> Api.create!()

      assert [%Post{title: "foo"}] = Api.read!(Post, authorize?: true)
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
        |> Api.create!()
      end

      good_post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "foo"})
        |> Api.create!()

      bad_post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "test"})
        |> Api.create!()

      # Filters apply to the base data
      assert_raise Ash.Error.Forbidden, fn ->
        bad_post
        |> Ash.Changeset.for_update(:update, %{title: "next"}, authorize?: true)
        |> Api.update!()
      end

      good_post
      |> Ash.Changeset.for_update(:update, %{title: "next"}, authorize?: true)
      |> Api.update!()
    end
  end
end
