# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.Subscription do
  @moduledoc """
  Test resource exercising identities that reference attributes with
  unusual names (trailing `?`). The manifest should surface the identity
  with its raw field-name keys intact.
  """
  use Ash.Resource,
    domain: Ash.Test.Manifest.Domain,
    data_layer: Ash.DataLayer.Ets

  ets do
    private? true
  end

  identities do
    identity :by_user_and_status, [:user_id, :is_active?],
      pre_check_with: Ash.Test.Manifest.Domain
  end

  attributes do
    integer_primary_key :id

    attribute :user_id, :uuid do
      allow_nil? false
      public? true
    end

    attribute :plan, :string do
      allow_nil? false
      public? true
    end

    attribute :is_active?, :boolean do
      default true
      public? true
    end

    attribute :is_trial?, :boolean do
      default false
      public? true
    end
  end

  actions do
    defaults [:read, :destroy]

    read :get_by_id do
      get_by :id
    end

    create :create do
      accept [:user_id, :plan, :is_active?, :is_trial?]

      change fn changeset, _context ->
        if Ash.Changeset.get_attribute(changeset, :id) do
          changeset
        else
          Ash.Changeset.force_change_attribute(changeset, :id, System.unique_integer([:positive]))
        end
      end
    end

    update :update do
      accept [:plan, :is_active?, :is_trial?]
    end
  end
end
