# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.EmbeddedArray.Company do
  @moduledoc false
  use Ash.Resource,
    domain: Ash.Test.EmbeddedArray.Domain,
    data_layer: Ash.DataLayer.Ets

  ets do
    private?(true)
  end

  actions do
    default_accept :*
    defaults [:read, :destroy, create: :*, update: :*]
  end

  attributes do
    uuid_primary_key :id, writable?: true
    attribute :name, :string, public?: true
  end

  relationships do
    has_many :estimates, Ash.Test.EmbeddedArray.Estimate do
      public? true
      destination_attribute :company_id
    end
  end
end
