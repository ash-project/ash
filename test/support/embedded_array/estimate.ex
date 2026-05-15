# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.EmbeddedArray.Estimate do
  @moduledoc false
  use Ash.Resource,
    domain: Ash.Test.EmbeddedArray.Domain,
    data_layer: Ash.DataLayer.Ets

  alias Ash.Test.EmbeddedArray.Option

  ets do
    private?(true)
  end

  actions do
    default_accept :*
    defaults [:read, :destroy, create: :*, update: :*]
  end

  attributes do
    uuid_primary_key :id, writable?: true
    attribute :title, :string, public?: true
    attribute :active, :boolean, public?: true, default: true
    attribute :options, {:array, Option}, public?: true, default: []
  end
end
