# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.UserSettings do
  @moduledoc """
  Test resource for user settings with multitenancy support.
  """
  use Ash.Resource,
    domain: Ash.Test.Manifest.Domain,
    data_layer: Ash.DataLayer.Ets,
    primary_read_warning?: false

  ets do
    private? true
  end

  multitenancy do
    strategy :attribute
    attribute :user_id
  end

  attributes do
    uuid_primary_key :id

    attribute :user_id, :uuid do
      allow_nil? false
      public? true
    end

    attribute :theme, :atom do
      constraints one_of: [:light, :dark, :auto]
      default :light
      public? true
    end

    attribute :language, :string do
      constraints max_length: 10
      default "en"
      public? true
    end

    attribute :notifications_enabled, :boolean do
      default true
      public? true
    end

    attribute :email_notifications, :boolean do
      default true
      public? true
    end

    attribute :timezone, :string do
      constraints max_length: 50
      default "UTC"
      public? true
    end

    attribute :date_format, :string do
      constraints max_length: 20
      default "YYYY-MM-DD"
      public? true
    end

    attribute :preferences, :map do
      default %{}
      public? true
    end

    create_timestamp :created_at do
      public? true
    end

    update_timestamp :updated_at do
      public? true
    end
  end

  relationships do
    belongs_to :user, Ash.Test.Manifest.User do
      allow_nil? false
      public? true
      attribute_writable? true
    end
  end

  actions do
    defaults [:read, :destroy]

    create :create do
      primary? true

      accept [
        :theme,
        :language,
        :notifications_enabled,
        :email_notifications,
        :timezone,
        :date_format,
        :preferences
      ]

      argument :user_id, :uuid do
        allow_nil? false
      end

      change manage_relationship(:user_id, :user, type: :append)
    end

    update :update do
      primary? true

      accept [
        :theme,
        :language,
        :notifications_enabled,
        :email_notifications,
        :timezone,
        :date_format,
        :preferences
      ]
    end

    read :get_by_user do
      get_by [:user_id]
    end
  end
end
