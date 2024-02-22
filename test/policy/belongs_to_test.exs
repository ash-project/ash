defmodule Ash.Test.Policy.Actions.BelongsToTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.AnyApi, as: Api

  defmodule Post do
    @moduledoc false
    use Ash.Resource, api: Api, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      defaults([:create, :read, :update, :destroy])

      update :update_with_reviewer do
        argument :reviewer_id, :uuid, allow_nil?: true
        change manage_relationship(:reviewer_id, :reviewer, type: :append_and_remove)
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, allow_nil?: false
    end

    relationships do
      belongs_to :reviewer, Ash.Test.Policy.Actions.BelongsToTest.Reviewer, allow_nil?: true
    end
  end

  defmodule Reviewer do
    @moduledoc false
    use Ash.Resource,
      api: Api,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private?(true)
    end

    actions do
      defaults [:create, :read, :update, :destroy]
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string, allow_nil?: false
    end

    policies do
      policy always() do
        authorize_if never()
      end
    end
  end

  test "update via manage_relationship fails when :read on related resource is not authorised" do
    reviewer =
      Reviewer
      |> Ash.Changeset.for_create(:create, %{name: "Zach"})
      |> Api.create!(authorize?: false)

    post =
      Post
      |> Ash.Changeset.for_create(:create, %{
        title: "A Post"
      })
      |> Api.create!()

    assert_raise Ash.Error.Forbidden, fn ->
      post
      |> Ash.Changeset.for_update(:update_with_reviewer, %{
        reviewer_id: reviewer.id
      })
      |> Api.update!()
    end
  end

  test "authorize?: false opt is passed through to the action" do
    reviewer =
      Reviewer
      |> Ash.Changeset.for_create(:create, %{name: "Zach"})
      |> Api.create!(authorize?: false)

    post =
      Post
      |> Ash.Changeset.for_create(:create, %{
        title: "A Post"
      })
      |> Api.create!()

    post
    |> Ash.Changeset.for_update(
      :update_with_reviewer,
      %{reviewer_id: reviewer.id},
      authorize?: false
    )
    |> Api.update!(authorize?: false)
  end
end
