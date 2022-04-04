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
    use Ash.Api

    resources do
      registry Registry
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

    test "a simple filter can also be applied to changesets" do
      start_supervised(
        {Ash.Test.Authorizer,
         strict_check: {:filter, [title: "foo"]}, strict_check_context: [:query, :changeset]}
      )

      # Filter always fails on creates
      assert_raise Ash.Error.Forbidden, fn ->
        Post
        |> Ash.Changeset.for_create(:create, %{title: "test"})
        |> Api.create!(authorize?: true)
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
        |> Ash.Changeset.for_update(:update, %{title: "next"})
        |> Api.update!(authorize?: true)
      end

      good_post
      |> Ash.Changeset.for_update(:update, %{title: "next"})
      |> Api.update!(authorize?: true)
    end
  end
end
