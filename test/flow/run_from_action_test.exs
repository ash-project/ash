defmodule Ash.Flow.RunFromAction do
  @moduledoc false
  use ExUnit.Case, async: false

  # https://github.com/ash-project/ash/issues/652
  test "Runs originated from resource action(Issue 652)" do
    parent_resource =
      Ash.Test.Flow.ParentResource
      |> Ash.Changeset.for_create(:create, %{status: :active})
      |> Ash.create!()

    Ash.Test.Flow.ChildResource
    |> Ash.Changeset.for_create(:create, %{parent_resource: parent_resource})
    |> Ash.create!(authorize?: false)

    Ash.Test.Flow.ChildResource
    |> Ash.Changeset.for_create(:create, %{parent_resource: parent_resource})
    |> Ash.create!(authorize?: false)

    assert %{status: :canceled} =
             Ash.Test.Flow.ParentResource.cancel!(parent_resource)
  end
end
