defmodule Ash.Test.RelationshipThroughTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule ParentResource do
    use Ash.Resource,
      domain: Ash.Test.Domain,
      data_layer: Ash.DataLayer.Ets

    actions do
      defaults [:read, :destroy, update: :*]

      create :create do
        primary? true
        accept [:name]
        argument :other_resources, {:array, :map}
        change manage_relationship(:other_resources, :other_resources, type: :create)
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string, public?: true
    end

    relationships do
      has_many :other_resources, Ash.Test.RelationshipThroughTest.OtherResource

      has_many :other_related_resources, Ash.Test.RelationshipThroughTest.RelatedResource,
        through: [:other_resources, :related_resource]

      has_many :other_many_related_resources, Ash.Test.RelationshipThroughTest.RelatedResource,
        through: [:other_resources, :related_resources]

      has_many :other_related_resources_invalid, Ash.Test.RelationshipThroughTest.RelatedResource,
        through: [:other_resources, :related_resource_invalid]
    end
  end

  defmodule OtherResource do
    use Ash.Resource,
      domain: Ash.Test.Domain,
      data_layer: Ash.DataLayer.Ets

    actions do
      defaults [:read, :destroy, update: :*]

      create :create do
        primary? true
        argument :related_resource, :map
        change manage_relationship(:related_resource, :related_resource, type: :create)
        argument :related_resources, {:array, :map}
        change manage_relationship(:related_resources, :related_resources, type: :create)
      end
    end

    attributes do
      uuid_primary_key :id
    end

    relationships do
      belongs_to :parent_resource, Ash.Test.RelationshipThroughTest.ParentResource
      has_one :related_resource, Ash.Test.RelationshipThroughTest.RelatedResource
      has_many :related_resources, Ash.Test.RelationshipThroughTest.RelatedResource
    end
  end

  defmodule RelatedResource do
    use Ash.Resource,
      domain: Ash.Test.Domain,
      data_layer: Ash.DataLayer.Ets

    actions do
      defaults [:read, :destroy, create: :*, update: :*]
    end

    attributes do
      uuid_primary_key :id
    end

    relationships do
      belongs_to :other_resource, Ash.Test.RelationshipThroughTest.OtherResource

      has_one :parent_resource, Ash.Test.RelationshipThroughTest.ParentResource,
        through: [:other_resource, :parent_resource]

      has_many :parent_other_resources, Ash.Test.RelationshipThroughTest.OtherResource,
        through: [:other_resource, :parent_resource, :other_resources]
    end
  end

  setup do
    other_params = %{related_resource: %{}, related_resources: [%{}]}
    params = %{name: "Parent", other_resources: [other_params]}
    parent = Ash.create!(Ash.Changeset.for_create(ParentResource, :create, params))
    %{other_resources: [%{related_resource: related_resource}]} = parent
    {:ok, %{parent: parent, related: related_resource}}
  end

  test "loads many through has_many has_many", %{parent: parent} do
    statement = [:other_many_related_resources, other_resources: :related_resources]
    parent = Ash.load!(parent, statement)

    assert Enum.map(parent.other_many_related_resources, & &1.id) ==
             Enum.flat_map(parent.other_resources, fn resource ->
               Enum.map(resource.related_resources, fn resource -> resource.id end)
             end)
  end

  test "loads many through has_many has_one", %{parent: parent} do
    statement = [:other_related_resources, other_resources: :related_resource]
    parent = Ash.load!(parent, statement)

    assert Enum.map(parent.other_related_resources, & &1.id) ==
             Enum.map(parent.other_resources, & &1.related_resource.id)
  end

  test "loads one through belongs_to belongs_to", %{parent: parent, related: related} do
    related = Ash.load!(related, :parent_resource)
    assert related.parent_resource.id == parent.id
  end

  test "loads many through belongs_to belongs_to has_many", %{parent: parent, related: related} do
    related = Ash.load!(related, :parent_other_resources)

    assert Enum.map(related.parent_other_resources, & &1.id) ==
             Enum.map(parent.other_resources, & &1.id)
  end

  test "load returns error when defined through is invalid", %{parent: parent} do
    assert {:error, %Ash.Error.Invalid{errors: [%Ash.Error.Query.InvalidLoad{}]}} =
             Ash.load(parent, [:other_related_resources_invalid])
  end
end
