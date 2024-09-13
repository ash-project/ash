defmodule Ash.Test.ManageRelationshipTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule ParentResource do
    use Ash.Resource,
      domain: Ash.Test.Domain

    actions do
      defaults [:read, :update, :destroy]

      create :create do
        accept [:name]
        argument :related_resource, :map, allow_nil?: false

        change manage_relationship(:related_resource, :related_resource, type: :create)
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

  defmodule RelatedResource do
    use Ash.Resource,
      domain: Ash.Test.Domain

    actions do
      defaults [:read, :destroy, create: :*, update: :*]
    end

    attributes do
      uuid_primary_key :id
      attribute :required_attribute, :string, allow_nil?: false
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
end
