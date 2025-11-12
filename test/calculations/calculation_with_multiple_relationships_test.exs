# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.CalculationWithMultipleRelationshipsTest do
  @moduledoc """
  Tests for calculations using multiple relationships to the same resource
  with different read actions.

  Bug: When two relationships are defined to the same resource with different
  read actions, calculations using each relationship end up using the SAME
  read action
  """
  use ExUnit.Case, async: true

  defmodule Item do
    use Ash.Resource,
      data_layer: Ash.DataLayer.Ets,
      domain: Ash.Test.Domain

    ets do
      private? true
    end

    actions do
      defaults create: :*

      read :read_active do
        primary? true
        filter expr(active == true)
      end

      read :read_all do
        # No filter - returns all records
      end
    end

    attributes do
      uuid_primary_key(:id)
      attribute(:name, :string, public?: true)
      attribute(:active, :boolean, public?: true, default: false)
    end

    relationships do
      belongs_to :container, Ash.Test.CalculationWithMultipleRelationshipsTest.Container do
        public? true
        allow_nil? false
      end

      belongs_to :container_reversed,
                 Ash.Test.CalculationWithMultipleRelationshipsTest.ContainerReversed do
        public? true
        allow_nil? false
      end
    end
  end

  defmodule Container do
    use Ash.Resource,
      data_layer: Ash.DataLayer.Ets,
      domain: Ash.Test.Domain

    ets do
      private? true
    end

    actions do
      default_accept :*
      defaults([:read, create: :*])
    end

    attributes do
      uuid_primary_key(:id)
    end

    calculations do
      calculate :active_item_name,
                :string,
                expr(item_active.name)

      calculate :all_item_name,
                :string,
                expr(item_all.name)
    end

    relationships do
      has_one :item_active, Item do
        public? true
      end

      has_one :item_all, Item do
        public? true
        read_action :read_all
      end
    end
  end

  test "calculations use correct read actions from their respective relationships" do
    container = Ash.Seed.seed!(Container, %{})

    item =
      Ash.Seed.seed!(Item, %{
        container_id: container.id,
        namme: "Inactive Item",
        active: false
      })

    loaded_container = Ash.load!(container, [:item_all])

    assert item.id == loaded_container.item_all.id

    loaded_container = Ash.load!(container, [:item_active])
    assert nil == loaded_container.item_active

    loaded_container = Ash.load!(container, [:active_item_name, :all_item_name])

    assert loaded_container.active_item_name == nil

    assert loaded_container.all_item_name == "Inactive Item"
  end

  test "loading calculations one at a time works" do
    container = Ash.Seed.seed!(Container, %{})

    item =
      Ash.Seed.seed!(Item, %{
        container_id: container.id,
        namme: "Inactive Item",
        active: false
      })

    loaded_container = Ash.load!(container, [:item_all])

    assert item.id == loaded_container.item_all.id

    loaded_container = Ash.load!(container, [:item_active])

    assert nil == loaded_container.item_active

    loaded_container = Ash.load!(container, [:active_item_name])

    assert loaded_container.active_item_name == nil

    loaded_container = Ash.load!(container, [:all_item_name])

    assert loaded_container.all_item_name == "Inactive Item"
  end
end
