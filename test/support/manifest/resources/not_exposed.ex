# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.NotExposed do
  @moduledoc """
  Test resource not exposed to manifest generation.
  """
  use Ash.Resource,
    domain: Ash.Test.Manifest.Domain,
    data_layer: Ash.DataLayer.Ets,
    primary_read_warning?: false

  ets do
    private? true
  end

  attributes do
    uuid_primary_key :id

    attribute :name, :string do
      allow_nil? false
      public? true
    end

    attribute :email, :string do
      allow_nil? false
      public? true
    end
  end

  relationships do
    belongs_to :todo, Ash.Test.Manifest.Todo do
      public? true
    end
  end

  actions do
    defaults [:read]

    create :create do
      accept [:email, :name]
    end

    update :update do
      accept [:name]
    end

    destroy :destroy do
      accept []
    end
  end
end
