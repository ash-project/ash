# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.User do
  @moduledoc """
  Test resource representing a user with relationships to todos and settings.
  """
  use Ash.Resource,
    domain: Ash.Test.Manifest.Domain,
    data_layer: Ash.DataLayer.Ets,
    primary_read_warning?: false

  ets do
    private? true
  end

  identities do
    identity :unique_email, [:email], pre_check_with: Ash.Test.Manifest.Domain
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

    attribute :active, :boolean do
      default true
      public? true
    end

    attribute :is_super_admin, :boolean do
      default false
      public? true
    end

    attribute :address_line_1, :string do
      allow_nil? true
      public? true
    end
  end

  relationships do
    has_many :comments, Ash.Test.Manifest.TodoComment do
      public? true
    end

    has_many :todos, Ash.Test.Manifest.Todo do
      public? true
    end

    has_many :posts, Ash.Test.Manifest.Post,
      destination_attribute: :author_id,
      public?: true
  end

  actions do
    defaults [:read]

    read :read_with_invalid_arg do
      argument :is_active?, :boolean
    end

    read :get_by_id do
      get_by :id
    end

    create :create do
      accept [:email, :name, :is_super_admin, :address_line_1]
    end

    update :update do
      accept [:name, :is_super_admin, :address_line_1]
    end

    update :update_me do
      description "Update the authenticated user's own information. Actor-scoped action."
      accept [:name, :address_line_1]
      require_atomic? false
      # This filter scopes the action to only update the actor's own record
      filter expr(id == ^actor(:id))
    end

    destroy :destroy do
      accept []
    end

    destroy :destroy_me do
      description "Delete the authenticated user's own account. Actor-scoped action."
      require_atomic? false
      filter expr(id == ^actor(:id))
    end
  end

  calculations do
    calculate :self, :struct, Ash.Test.Manifest.SelfCalculation do
      constraints instance_of: __MODULE__
      public? true

      argument :prefix, :string do
        allow_nil? true
        default nil
      end
    end

    calculate :is_active?, :boolean, expr(true) do
      public? true
    end
  end
end
