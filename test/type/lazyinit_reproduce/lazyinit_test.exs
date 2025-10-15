# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Type.LazyInitTest do
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule Post do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, update: :*]

      create :create do
        primary? true
        accept [:*]

        argument :dummy_metadata, :struct, constraints: [instance_of: Metadata], allow_nil?: true
      end

      read :search do
        argument :predicate, Type.LazyInitTest.Example
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string
    end
  end

  test "lazy init" do
    assert {:ok, true} = {:ok, true}
  end
end
