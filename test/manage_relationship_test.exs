defmodule Ash.Test.ManageRelationshipTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule ParentResource do
    use Ash.Resource,
      domain: Ash.Test.Domain,
      data_layer: Ash.DataLayer.Ets

    actions do
      defaults [:read, :destroy]

      create :create do
        primary? true
        accept [:name]
        argument :related_resource, :map
        argument :other_resources, {:array, :map}

        change manage_relationship(:related_resource, :related_resource, type: :create)
        change manage_relationship(:other_resources, type: :direct_control)
      end

      create :create_ordered do
        accept [:name]
        argument :other_resources, {:array, :map}

        change manage_relationship(:other_resources, type: :direct_control, order_is_key: :order)
      end

      update :update do
        require_atomic? false
        primary? true
        accept [:name]
        argument :related_resource, :map
        argument :other_resources, {:array, :map}

        change manage_relationship(:related_resource, :related_resource, type: :direct_control)
        change manage_relationship(:other_resources, type: :direct_control)
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string
    end

    relationships do
      has_one :related_resource, Ash.Test.ManageRelationshipTest.RelatedResource,
        destination_attribute: :parent_resource_id

      has_many :other_resources, Ash.Test.ManageRelationshipTest.OtherResource
    end
  end

  defmodule RelatedResourceFragment do
    use Spark.Dsl.Fragment, of: Ash.Resource

    actions do
      read :another_read
    end
  end

  defmodule RelatedResource do
    use Ash.Resource,
      domain: Ash.Test.Domain,
      data_layer: Ash.DataLayer.Ets,
      fragments: [RelatedResourceFragment]

    actions do
      defaults [:read, :destroy, create: :*, update: :*]

      create :create_with_parent do
        accept [:required_attribute]
        argument :parent_resource, :map, allow_nil?: false
        change manage_relationship(:parent_resource, type: :direct_control)

        change after_action(fn changeset, result, context ->
                 if !changeset.attributes[:parent_resource_id] do
                   raise "should have a parent_resource_id"
                 end

                 {:ok, result}
               end)
      end

      create :create_with_existing_parent do
        accept [:required_attribute]
        argument :parent_resource, :map, allow_nil?: false

        change manage_relationship(:parent_resource, on_lookup: :relate_and_update)
      end

      update :update_with_existing_parent do
        argument :parent_resource, :map, allow_nil?: false
        require_atomic? false

        change manage_relationship(:parent_resource, on_match: :update)
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :required_attribute, :string, allow_nil?: false, public?: true
    end

    relationships do
      belongs_to :parent_resource, ParentResource
    end
  end

  defmodule OtherResource do
    use Ash.Resource,
      domain: Ash.Test.Domain,
      data_layer: Ash.DataLayer.Ets,
      fragments: [RelatedResourceFragment]

    actions do
      defaults [:read, create: :*, update: :*]

      destroy :archive do
        primary? true
        soft? true
        change set_attribute(:archived_at, &DateTime.utc_now/0)
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :required_attribute, :string, allow_nil?: false, public?: true
      attribute :archived_at, :utc_datetime_usec
      attribute :order, :integer, public?: true
    end

    relationships do
      belongs_to :parent_resource, ParentResource
    end
  end

  test "errors have the proper path set on them" do
    assert {:error,
            %Ash.Error.Invalid{
              errors: [
                %{path: [:related_resource, 0]}
              ]
            }} =
             ParentResource
             |> Ash.Changeset.for_create(:create, %{
               name: "Test Parent Resource",
               related_resource: %{
                 # This is the missing required field
                 required_attribute: nil
               }
             })
             |> Ash.create()
  end

  test "order_is_key sets the order accordingly" do
    assert %{other_resources: [%{order: 0}, %{order: 1}]} =
             ParentResource
             |> Ash.Changeset.for_create(:create_ordered, %{
               name: "Test Parent Resource",
               other_resources: [
                 %{
                   required_attribute: "string"
                 },
                 %{
                   required_attribute: "string"
                 }
               ]
             })
             |> Ash.create!()
  end

  test "can create a belongs_to relationship, and observe the attribute change" do
    assert {:ok, related} =
             RelatedResource
             |> Ash.Changeset.for_create(:create_with_parent, %{
               required_attribute: "string",
               parent_resource: %{
                 name: "Test Parent Resource"
               }
             })
             |> Ash.create!()
             |> Ash.load(:parent_resource)

    assert related.parent_resource.name == "Test Parent Resource"
  end

  test "relates and updates existing parent" do
    parent =
      ParentResource
      |> Ash.Changeset.for_create(:create, %{name: "Initial name"})
      |> Ash.create!()

    {:ok, related} =
      RelatedResource
      |> Ash.Changeset.for_create(:create_with_existing_parent, %{
        required_attribute: "string",
        parent_resource: %{id: parent.id, name: "New name"}
      })
      |> Ash.create!()
      |> Ash.load(:parent_resource)

    assert related.parent_resource.name == "New name"

    {:ok, related} =
      related
      |> Ash.Changeset.for_update(:update_with_existing_parent, %{
        parent_resource: %{id: parent.id, name: "Even newer name"}
      })
      |> Ash.update!()
      |> Ash.load(:parent_resource)

    assert related.parent_resource.name == "Even newer name"
  end

  test "can create and update a related resource" do
    assert {:ok, parent} =
             ParentResource
             |> Ash.Changeset.for_create(:create, %{
               name: "Test Parent Resource",
               related_resource: %{
                 required_attribute: "string"
               }
             })
             |> Ash.create!()
             |> Ash.load(:related_resource)

    assert parent.related_resource.required_attribute == "string"

    assert {:ok, parent} =
             parent
             |> Ash.Changeset.for_update(:update, %{
               related_resource: %{
                 id: parent.related_resource.id,
                 required_attribute: "other_string"
               }
             })
             |> Ash.update!()
             |> Ash.load(:related_resource)

    assert parent.related_resource.required_attribute == "other_string"
  end

  test "can create and destroy arrays" do
    assert {:ok, parent} =
             ParentResource
             |> Ash.Changeset.for_create(:create, %{
               name: "Test Parent Resource",
               other_resources: [
                 %{required_attribute: "first"},
                 %{required_attribute: "second"}
               ]
             })
             |> Ash.create!()
             |> Ash.load(:other_resources)

    assert Enum.map(parent.other_resources, & &1.required_attribute) |> Enum.sort() == [
             "first",
             "second"
           ]

    other_resources = Enum.reject(parent.other_resources, &(&1.required_attribute == "first"))

    assert {:ok, updated_parent} =
             parent
             |> Ash.Changeset.for_update(:update, %{
               other_resources: other_resources
             })
             |> Ash.update!()
             |> Ash.load(:other_resources)

    # Since we did a soft destroy, we should still have all other resources
    assert Enum.map(updated_parent.other_resources, & &1.required_attribute) |> Enum.sort() == [
             "first",
             "second"
           ]

    first_other = Enum.find(updated_parent.other_resources, &(&1.required_attribute == "first"))
    assert not is_nil(first_other.archived_at)

    second_other = Enum.find(updated_parent.other_resources, &(&1.required_attribute == "second"))
    assert is_nil(second_other.archived_at)
  end
end
