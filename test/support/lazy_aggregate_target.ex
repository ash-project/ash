# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Support.LazyAggregateTarget do
  @moduledoc """
  Resource used to test unrelated aggregates whose target has not been loaded
  yet. Lives in test/support (compiled to disk) so tests can `:code.delete/1`
  it and the VM can load it again.
  """
  use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

  ets do
    private? true
  end

  attributes do
    uuid_primary_key :id
    attribute :name, :string, public?: true
  end

  actions do
    defaults [:read, create: :*]
  end
end
