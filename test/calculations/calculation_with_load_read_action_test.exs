# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.CalculationWithLoadReadActionTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule Related do
    use Ash.Resource,
      data_layer: Ash.DataLayer.Ets,
      domain: Ash.Test.Domain

    ets do
      private? true
    end

    actions do
      default_accept :*
      defaults([:read, create: :*])

      read :read_active do
        filter expr(active == true)
      end

      read :read_all do
      end
    end

    attributes do
      uuid_primary_key(:id)
      attribute(:name, :string, public?: true)
      attribute(:active, :boolean, public?: true, default: false)
    end

    relationships do
      belongs_to :parent, Ash.Test.CalculationWithLoadReadActionTest.Parent do
        public? true
        allow_nil? false
      end
    end
  end

  defmodule Parent do
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
      calculate :calculated_common_name,
                :string,
                expr(related.name),
                load: [
                  related: Ash.Query.for_read(Related, :read_all)
                ]
    end

    relationships do
      has_one :related, Related do
        public? true
        destination_attribute :parent_id
        # The default read action for this relationship filters to active only
        read_action :read_active
      end
    end
  end

  test "works when the resource is visible for both read actions" do
    parent = Ash.Seed.seed!(Parent, %{})

    Ash.Seed.seed!(Related, %{
      parent_id: parent.id,
      name: "Active Record",
      active: true
    })

    loaded_parent = Ash.load!(parent, :calculated_common_name)

    assert loaded_parent.calculated_common_name == "Active Record"
  end

  test "calculation uses read action specified in load, not relationship default" do
    parent = Ash.Seed.seed!(Parent, %{})

    Ash.Seed.seed!(Related, %{
      parent_id: parent.id,
      name: "Inactive Record",
      active: false
    })

    loaded_parent = Ash.load!(parent, :calculated_common_name)

    assert loaded_parent.calculated_common_name == "Inactive Record"
  end
end
