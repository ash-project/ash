defmodule Ash.Test.Actions.LoadIssueTest do
  @moduledoc false
  use ExUnit.Case, async: false
  @moduletag :wip
  require Ash.Query

  defmodule Parent do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      defaults [:create, :read]
    end

    attributes do
      uuid_primary_key :id
    end

    relationships do
      has_many :children, Ash.Test.Actions.LoadIssueTest.Child, destination_field: :parent_id
    end
  end

  defmodule Child do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      defaults [:read, :create, :update, :destroy]

      create :for_parent do
        argument(:parent_id, :uuid, allow_nil?: false)

        change manage_relationship(:parent_id, :parent, type: :replace)
      end
    end

    attributes do
      uuid_primary_key :id
    end

    relationships do
      belongs_to :parent, Ash.Test.Actions.LoadIssueTest.Parent
    end
  end

  defmodule Registry do
    @moduledoc false
    use Ash.Registry

    entries do
      entry(Parent)
      entry(Child)
    end
  end

  defmodule Api do
    @moduledoc false
    use Ash.Api

    resources do
      registry Registry
    end
  end

  describe "issue with load" do
    setup do
      parent =
        Parent
        |> Ash.Changeset.for_create(:create)
        |> Api.create!()

      %{parent: parent}
    end

    test "fails: load children, create child and load children again", %{parent: parent} do
      parent = Api.load!(parent, :children)

      Child
      |> Ash.Changeset.for_create(:for_parent, %{parent_id: parent.id})
      |> Api.create!()

      parent =
        parent
        |> Api.load!(:children)

      assert Enum.count(parent.children) == 1
    end

    test "works: load children, create child, reload parent and load again", %{parent: parent} do
      parent = Api.load!(parent, :children)

      Child
      |> Ash.Changeset.for_create(:for_parent, %{parent_id: parent.id})
      |> Api.create!()

      parent =
        parent
        |> Api.reload!()
        |> Api.load!(:children)

      assert Enum.count(parent.children) == 1
    end
  end
end
