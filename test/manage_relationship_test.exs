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

        upsert? true
        upsert_identity :uniqe_value
        upsert_fields [:name]

        accept [:name]
        argument :related_resource, :map
        argument :other_resources, {:array, :map}

        change manage_relationship(:related_resource, :related_resource,
                 type: :create,
                 use_identities: [:by_parent_resource_id]
               )

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

      update :create_has_one_child do
      end
    end

    identities do
      identity :uniqe_value, [:unique_value], pre_check_with: Ash.Test.Domain
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string

      attribute :unique_value, :uuid do
        allow_nil? false
        default &Ash.UUIDv7.generate/0
      end
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

    identities do
      identity :by_parent_resource_id, [:parent_resource_id], pre_check_with: Ash.Test.Domain
    end

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
      defaults [:read, :destroy, create: :*, update: :*]

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

  setup do
    on_exit(fn ->
      RelatedResource
      |> Ash.Query.for_read(:read, %{}, authorize?: false)
      |> Ash.bulk_destroy!(:destroy, %{}, authorize?: false)

      ParentResource
      |> Ash.Query.for_read(:read, %{}, authorize?: false)
      |> Ash.bulk_destroy!(:destroy, %{}, authorize?: false)

      OtherResource
      |> Ash.Query.for_read(:read, %{}, authorize?: false)
      |> Ash.bulk_destroy!(:destroy, %{}, authorize?: false)
    end)
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

  test "can upsert without createing a new related resource" do
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
             ParentResource
             |> Ash.Changeset.for_create(:create, %{
               unique_value: parent.unique_value,
               name: "Test Parent Resource",
               related_resource: %{
                 required_attribute: "string2"
               }
             })
             |> Ash.create!()
             |> Ash.load(:related_resource)

    assert parent.related_resource.required_attribute == "string"

    assert {:ok, [_related]} =
             RelatedResource
             |> Ash.Query.for_read(:read, %{}, authorize?: false)
             |> Ash.read()
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
