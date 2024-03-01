defmodule Ash.Test.Resource.Changes.SetNewAttributeTest do
  @moduledoc false

  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule Resource do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    attributes do
      uuid_primary_key :id

      attribute :name, :string do
        public?(true)
      end
    end

    actions do
      defaults [:read]

      create :create do
        change set_new_attribute(:name, "default_name")
      end
    end
  end

  test "set_new_attributes set attribute when it's not already changing" do
    %Resource{name: "default_name"} =
      Resource |> Ash.Changeset.for_create(:create) |> Domain.create!()
  end

  test "set_new_attribute does not set attribute when it's already changing" do
    %Resource{name: "specific_name"} =
      Resource |> Ash.Changeset.for_create(:create, %{name: "specific_name"}) |> Domain.create!()
  end
end
