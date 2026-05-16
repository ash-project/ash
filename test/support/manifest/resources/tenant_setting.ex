# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.TenantSetting do
  @moduledoc """
  Test resource for verifying composite primary key handling.

  This resource uses a composite primary key (tenant_id + setting_key) to test
  that identity types are correctly generated for both regular action functions
  (using actual types) and validation functions (using string types).
  """
  use Ash.Resource,
    domain: Ash.Test.Manifest.Domain,
    data_layer: Ash.DataLayer.Ets

  ets do
    private? true
  end

  attributes do
    attribute :tenant_id, :uuid do
      allow_nil? false
      public? true
      primary_key? true
    end

    attribute :setting_key, :string do
      allow_nil? false
      public? true
      primary_key? true
    end

    attribute :value, :string do
      allow_nil? false
      public? true
    end

    attribute :description, :string do
      public? true
    end
  end

  actions do
    defaults [:read, :destroy]

    create :create do
      accept [:tenant_id, :setting_key, :value, :description]
    end

    update :update do
      accept [:value, :description]
    end
  end
end
