# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

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
        argument :many_to_many_resources, {:array, :map}

        change manage_relationship(:related_resource, :related_resource, type: :create)
        change manage_relationship(:other_resources, type: :direct_control)
        change manage_relationship(:many_to_many_resources, type: :direct_control)
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

      update :update_many_soft do
        require_atomic? false
        argument :many_to_many_resources, {:array, :map}

        change manage_relationship(:many_to_many_resources,
                 type: :direct_control,
                 on_missing: {:destroy, :destroy_soft, :destroy_soft}
               )
      end

      update :update_many_hard do
        require_atomic? false
        argument :many_to_many_resources, {:array, :map}

        change manage_relationship(:many_to_many_resources,
                 type: :direct_control,
                 on_missing: {:destroy, :destroy_hard, :destroy_hard}
               )
      end

      update :remove_other_resource do
        require_atomic? false
        argument :other_resource_id, :uuid

        change manage_relationship(:other_resource_id, :other_resources, type: :remove)
      end

      update :remove_other_resource_ignore do
        require_atomic? false
        argument :other_resource_id, :uuid

        change manage_relationship(:other_resource_id, :other_resources,
                 on_no_match: :ignore,
                 on_match: :unrelate
               )
      end

      update :append_other_resource do
        require_atomic? false
        argument :other_resource_id, :uuid

        change manage_relationship(:other_resource_id, :other_resources, type: :append)
      end

      update :unrelate_with_action do
        require_atomic? false
        argument :other_resource_id, :uuid

        change manage_relationship(:other_resource_id, :other_resources,
                 on_no_match: :error,
                 on_match: {:unrelate, :update}
               )
      end

      update :destroy_with_action do
        require_atomic? false
        argument :other_resource_id, :uuid

        change manage_relationship(:other_resource_id, :other_resources,
                 on_no_match: :error,
                 on_match: {:destroy, :destroy}
               )
      end

      update :create_with_lookup_ignore do
        require_atomic? false
        argument :other_resource_id, :uuid

        change manage_relationship(:other_resource_id, :other_resources,
                 on_lookup: :ignore,
                 on_no_match: :error,
                 on_match: :ignore
               )
      end

      update :update_other_resource do
        require_atomic? false
        argument :other_resource_id, :uuid

        change manage_relationship(:other_resource_id, :other_resources,
                 on_no_match: :error,
                 on_match: :update
               )
      end

      update :update_other_resource_with_action do
        require_atomic? false
        argument :other_resource_id, :uuid

        change manage_relationship(:other_resource_id, :other_resources,
                 on_no_match: :error,
                 on_match: {:update, :update}
               )
      end
    end

    changes do
      change fn changeset, context ->
        if changeset.context[:fail?] do
          throw({:fail, context})
        else
          changeset
        end
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

      has_many :join_resources, Ash.Test.ManageRelationshipTest.JoinResource

      many_to_many :many_to_many_resources, Ash.Test.ManageRelationshipTest.ManyToManyResource do
        join_relationship :join_resources
      end
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

  defmodule JoinResource do
    use Ash.Resource,
      domain: Ash.Test.Domain,
      data_layer: Ash.DataLayer.Ets

    actions do
      defaults [:read, create: :*, update: :*]

      destroy :destroy_soft do
        soft? true
        change set_attribute(:archived_at, &DateTime.utc_now/0)
      end

      destroy :destroy_hard do
        primary? true
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :archived_at, :utc_datetime_usec
    end

    relationships do
      belongs_to :parent_resource, Ash.Test.ManageRelationshipTest.ParentResource
      belongs_to :many_to_many_resource, Ash.Test.ManageRelationshipTest.ManyToManyResource
    end
  end

  defmodule ManyToManyResource do
    use Ash.Resource,
      domain: Ash.Test.Domain,
      data_layer: Ash.DataLayer.Ets

    actions do
      defaults [:read, create: :*, update: :*]

      destroy :destroy_soft do
        soft? true
        change set_attribute(:archived_at, &DateTime.utc_now/0)
      end

      destroy :destroy_hard do
        primary? true
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string, allow_nil?: false, public?: true
      attribute :archived_at, :utc_datetime_usec
    end

    relationships do
      has_many :join_resources, JoinResource

      many_to_many :parent_resources, ParentResource do
        join_relationship :join_resources
      end
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

  test "passes shared context down" do
    parent =
      ParentResource
      |> Ash.Changeset.for_create(:create, %{name: "Initial name"})
      |> Ash.create!()

    try do
      RelatedResource
      |> Ash.Changeset.for_create(
        :create_with_existing_parent,
        %{
          required_attribute: "string",
          parent_resource: %{id: parent.id, name: "New name"}
        },
        context: %{shared: %{fail?: true}}
      )
      |> Ash.create!()

      flunk()
    catch
      {:fail, context} ->
        assert %Ash.Resource.Change.Context{
                 source_context: %{
                   shared: %{fail?: true},
                   fail?: true
                 }
               } = context
    end
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

  test "can hard-delete many-to-many relationships" do
    assert {:ok, parent} =
             ParentResource
             |> Ash.Changeset.for_create(:create, %{
               name: "Test Parent Resource",
               many_to_many_resources: [%{name: "first"}, %{name: "second"}]
             })
             |> Ash.create!()
             |> Ash.load([:many_to_many_resources])

    assert Enum.map(parent.many_to_many_resources, & &1.name) |> Enum.sort() == [
             "first",
             "second"
           ]

    second_id = Enum.find(parent.many_to_many_resources, &(&1.name == "second")).id

    assert {:ok, updated_parent} =
             parent
             |> Ash.Changeset.for_update(:update_many_hard, %{
               many_to_many_resources: [
                 %{id: second_id, name: "second"},
                 %{name: "third"}
               ]
             })
             |> Ash.update!()
             |> Ash.load(many_to_many_resources: :join_resources)

    first = Enum.find(updated_parent.many_to_many_resources, &(&1.name == "first"))
    assert first == nil

    second = Enum.find(updated_parent.many_to_many_resources, &(&1.name == "second"))
    assert is_nil(second.archived_at)

    third = Enum.find(updated_parent.many_to_many_resources, &(&1.name == "third"))
    assert is_nil(third.archived_at)
  end

  test "can soft-delete many-to-many relationships" do
    assert {:ok, parent} =
             ParentResource
             |> Ash.Changeset.for_create(:create, %{
               name: "Test Parent Resource",
               many_to_many_resources: [%{name: "first"}, %{name: "second"}]
             })
             |> Ash.create!()
             |> Ash.load([:many_to_many_resources])

    assert Enum.map(parent.many_to_many_resources, & &1.name) |> Enum.sort() == [
             "first",
             "second"
           ]

    second_id = Enum.find(parent.many_to_many_resources, &(&1.name == "second")).id

    assert {:ok, updated_parent} =
             parent
             |> Ash.Changeset.for_update(:update_many_soft, %{
               many_to_many_resources: [
                 %{id: second_id, name: "second"},
                 %{name: "third"}
               ]
             })
             |> Ash.update!()
             |> Ash.load(many_to_many_resources: :join_resources)

    first = Enum.find(updated_parent.many_to_many_resources, &(&1.name == "first"))
    assert not is_nil(first.archived_at)
    assert not is_nil(hd(first.join_resources).archived_at)

    second = Enum.find(updated_parent.many_to_many_resources, &(&1.name == "second"))
    assert is_nil(second.archived_at)
    assert is_nil(hd(second.join_resources).archived_at)

    third = Enum.find(updated_parent.many_to_many_resources, &(&1.name == "third"))
    assert is_nil(third.archived_at)
    assert is_nil(hd(third.join_resources).archived_at)
  end

  test "removing non-existent relationship returns NotFound error" do
    parent =
      ParentResource
      |> Ash.Changeset.for_create(:create, %{name: "Test Parent"})
      |> Ash.create!()

    non_existent_id = Ash.UUID.generate()

    assert {:error,
            %Ash.Error.Invalid{
              errors: [%Ash.Error.Query.NotFound{} = error | _]
            }} =
             parent
             |> Ash.Changeset.for_update(:remove_other_resource, %{
               other_resource_id: non_existent_id
             })
             |> Ash.update()

    assert Exception.message(error) =~ "record with id: #{inspect(non_existent_id)} not found"
  end

  test "removing non-existent relationship with on_no_match: :ignore succeeds" do
    parent =
      ParentResource
      |> Ash.Changeset.for_create(:create, %{name: "Test Parent"})
      |> Ash.create!()

    non_existent_id = Ash.UUID.generate()

    assert {:ok, updated_parent} =
             parent
             |> Ash.Changeset.for_update(:remove_other_resource_ignore, %{
               other_resource_id: non_existent_id
             })
             |> Ash.update()

    assert updated_parent.id == parent.id
  end

  test "append operation with non-existent relationship returns NotFound error" do
    parent =
      ParentResource
      |> Ash.Changeset.for_create(:create, %{name: "Test Parent"})
      |> Ash.create!()

    non_existent_id = Ash.UUID.generate()

    assert {:error,
            %Ash.Error.Invalid{
              errors: [%Ash.Error.Query.NotFound{} = error | _]
            }} =
             parent
             |> Ash.Changeset.for_update(:append_other_resource, %{
               other_resource_id: non_existent_id
             })
             |> Ash.update()

    assert Exception.message(error) =~ "record with id: #{inspect(non_existent_id)} not found"
  end

  test "unrelate with action tuple returns NotFound error" do
    parent =
      ParentResource
      |> Ash.Changeset.for_create(:create, %{name: "Test Parent"})
      |> Ash.create!()

    non_existent_id = Ash.UUID.generate()

    assert {:error,
            %Ash.Error.Invalid{
              errors: [%Ash.Error.Query.NotFound{} = error | _]
            }} =
             parent
             |> Ash.Changeset.for_update(:unrelate_with_action, %{
               other_resource_id: non_existent_id
             })
             |> Ash.update()

    assert Exception.message(error) =~ "record with id: #{inspect(non_existent_id)} not found"
  end

  test "destroy with action tuple returns NotFound error" do
    parent =
      ParentResource
      |> Ash.Changeset.for_create(:create, %{name: "Test Parent"})
      |> Ash.create!()

    non_existent_id = Ash.UUID.generate()

    assert {:error,
            %Ash.Error.Invalid{
              errors: [%Ash.Error.Query.NotFound{} = error | _]
            }} =
             parent
             |> Ash.Changeset.for_update(:destroy_with_action, %{
               other_resource_id: non_existent_id
             })
             |> Ash.update()

    assert Exception.message(error) =~ "record with id: #{inspect(non_existent_id)} not found"
  end

  test "create operation with on_lookup ignore shows create error message" do
    parent =
      ParentResource
      |> Ash.Changeset.for_create(:create, %{name: "Test Parent"})
      |> Ash.create!()

    non_existent_id = Ash.UUID.generate()

    assert {:error,
            %Ash.Error.Invalid{
              errors: [%Ash.Error.Changes.InvalidRelationship{} = error]
            }} =
             parent
             |> Ash.Changeset.for_update(:create_with_lookup_ignore, %{
               other_resource_id: non_existent_id
             })
             |> Ash.update()

    assert Exception.message(error) =~
             "Invalid value provided for other_resources: changes would create a new related record."
  end

  test "update operation with missing relationship returns NotFound error" do
    parent =
      ParentResource
      |> Ash.Changeset.for_create(:create, %{name: "Test Parent"})
      |> Ash.create!()

    non_existent_id = Ash.UUID.generate()

    assert {:error,
            %Ash.Error.Invalid{
              errors: [%Ash.Error.Query.NotFound{} = error | _]
            }} =
             parent
             |> Ash.Changeset.for_update(:update_other_resource, %{
               other_resource_id: non_existent_id
             })
             |> Ash.update()

    assert Exception.message(error) =~ "record with id: #{inspect(non_existent_id)} not found"
  end

  test "update with action tuple returns NotFound error" do
    parent =
      ParentResource
      |> Ash.Changeset.for_create(:create, %{name: "Test Parent"})
      |> Ash.create!()

    non_existent_id = Ash.UUID.generate()

    assert {:error,
            %Ash.Error.Invalid{
              errors: [%Ash.Error.Query.NotFound{} = error | _]
            }} =
             parent
             |> Ash.Changeset.for_update(:update_other_resource_with_action, %{
               other_resource_id: non_existent_id
             })
             |> Ash.update()

    assert Exception.message(error) =~ "record with id: #{inspect(non_existent_id)} not found"
  end
end
