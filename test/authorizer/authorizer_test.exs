defmodule Ash.Test.Changeset.AuthorizerTest do
  @moduledoc false
  use ExUnit.Case, async: false

  require Ash.Query

  defmodule Post do
    use Ash.Resource,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [
        Ash.Test.Authorizer
      ],
      domain: Ash.Test.Changeset.AuthorizerTest.Domain

    ets do
      private? true
    end

    actions do
      defaults [:create, :read, :update, :destroy]

      create :title_is_authorization do
        accept []

        change fn changeset, context ->
          Ash.Changeset.change_attribute(changeset, :title, context.authorize?)
        end
      end
    end

    attributes do
      uuid_primary_key :id

      attribute :title, :string, allow_nil?: false, public?: true
    end
  end

  defmodule Domain do
    use Ash.Domain, otp_app: :ash

    resources do
      resource Post
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
        |> Domain.create!()
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
        |> Domain.create!()

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
        |> Domain.create!()
      end

      assert_raise Ash.Error.Forbidden, fn ->
        Post
        |> Ash.Changeset.for_create(:create, %{title: "test"})
        |> Domain.create!(actor: nil)
      end

      assert_raise Ash.Error.Forbidden, fn ->
        Ash.set_actor(nil)

        Post
        |> Ash.Changeset.for_create(:create, %{title: "test"})
        |> Domain.create!()
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
      |> Domain.create!(authorize?: false)

      Post
      |> Ash.Changeset.for_create(:create, %{title: "foo"})
      |> Domain.create!(authorize?: false)

      assert [%Post{title: "foo"}] = Domain.read!(Post, authorize?: true)
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
        |> Domain.create!()
      end

      good_post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "foo"}, authorize?: false)
        |> Domain.create!()

      bad_post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "test"}, authorize?: false)
        |> Domain.create!()

      # Filters apply to the base data
      assert_raise Ash.Error.Forbidden, fn ->
        bad_post
        |> Ash.Changeset.for_update(:update, %{title: "next"}, authorize?: true)
        |> Domain.update!()
      end

      good_post
      |> Ash.Changeset.for_update(:update, %{title: "next"}, authorize?: true)
      |> Domain.update!()
    end
  end
end
