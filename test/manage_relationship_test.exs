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
        accept [:name]
        argument :related_resource, :map, allow_nil?: false

        change manage_relationship(:related_resource, :related_resource, type: :create)
      end

      update :update do
        require_atomic? false
        accept [:name]
        argument :related_resource, :map, allow_nil?: false

        change manage_relationship(:related_resource, :related_resource, type: :direct_control)
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string
    end

    relationships do
      has_one :related_resource, Ash.Test.ManageRelationshipTest.RelatedResource,
        destination_attribute: :parent_resource_id
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
    end

    attributes do
      uuid_primary_key :id
      attribute :required_attribute, :string, allow_nil?: false, public?: true
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
end
