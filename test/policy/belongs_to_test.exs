defmodule Ash.Test.Policy.Actions.BelongsToTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule Post do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults([:read, :destroy, create: :*, update: :*])

      update :update_with_reviewer do
        require_atomic? false
        argument :reviewer_id, :uuid, allow_nil?: true
        change manage_relationship(:reviewer_id, :reviewer, type: :append_and_remove)
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, allow_nil?: false, public?: true
    end

    relationships do
      belongs_to :reviewer, Ash.Test.Policy.Actions.BelongsToTest.Reviewer,
        allow_nil?: true,
        public?: true
    end
  end

  defmodule Reviewer do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string, allow_nil?: false, public?: true
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
      |> Ash.create!(authorize?: false)

    post =
      Post
      |> Ash.Changeset.for_create(:create, %{
        title: "A Post"
      })
      |> Ash.create!()

    assert_raise Ash.Error.Invalid, ~r/not found/, fn ->
      post
      |> Ash.Changeset.for_update(:update_with_reviewer, %{
        reviewer_id: reviewer.id
      })
      |> Ash.update!()
    end
  end

  test "authorize?: false opt is passed through to the action" do
    reviewer =
      Reviewer
      |> Ash.Changeset.for_create(:create, %{name: "Zach"})
      |> Ash.create!(authorize?: false)

    post =
      Post
      |> Ash.Changeset.for_create(:create, %{
        title: "A Post"
      })
      |> Ash.create!()

    post
    |> Ash.Changeset.for_update(
      :update_with_reviewer,
      %{reviewer_id: reviewer.id},
      authorize?: false
    )
    |> Ash.update!(authorize?: false)
  end
end
