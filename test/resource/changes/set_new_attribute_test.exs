defmodule Ash.Test.Resource.Changes.SetNewAttributeTest do
  @moduledoc false

  use ExUnit.Case, async: true

  defmodule Resource do
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    attributes do
      uuid_primary_key :id

      attribute :name, :string
    end

    actions do
      defaults [:read]

      actions do
        create :create do
          change set_new_attribute(:name, "default_name")
        end
      end
    end
  end

  defmodule Registry do
    @moduledoc false
    use Ash.Registry

    entries do
      entry Resource
    end
  end

  defmodule Api do
    use Ash.Api

    resources do
      registry Registry
    end
  end

  test "set_new_attributes set attribute when it's not already changing" do
    %Resource{name: "default_name"} =
      Resource |> Ash.Changeset.for_create(:create) |> Api.create!()
  end

  test "set_new_attribute does not set attribute when it's already changing" do
    %Resource{name: "specific_name"} =
      Resource |> Ash.Changeset.for_create(:create, %{name: "specific_name"}) |> Api.create!()
  end
end
