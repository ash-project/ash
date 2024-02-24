defmodule Ash.Flow.RunFromAction do
  @moduledoc false
  use ExUnit.Case, async: false

  alias Ash.Test.Flow.Domain
  alias Ash.Test.Flow.ParentResource

  @tag :"652"
  # https://github.com/ash-project/ash/issues/652
  test "Runs originated from resource action(Issue 652)" do
    parent_resource =
      ParentResource
      |> Ash.Changeset.for_create(:create, %{status: :active})
      |> Domain.create!()

    Ash.Test.Flow.ChildResource
    |> Ash.Changeset.for_create(:create, %{parent_resource: parent_resource})
    |> Domain.create!(authorize?: false)

    Ash.Test.Flow.ChildResource
    |> Ash.Changeset.for_create(:create, %{parent_resource: parent_resource})
    |> Domain.create!(authorize?: false)

    assert %{status: :canceled} =
             ParentResource.cancel!(parent_resource)
  end
end
