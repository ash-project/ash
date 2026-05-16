# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.TaskMetadata do
  @moduledoc """
  Test embedded resource.

  This resource exercises:
  - `created_by?` -> `created_by` (removing question mark)
  - `is_public?` -> `is_public` (removing question mark)
  """

  use Ash.Resource,
    data_layer: :embedded,
    domain: nil

  attributes do
    uuid_primary_key :id

    attribute :notes, :string do
      public? true
      allow_nil? true
    end

    attribute :created_by?, :string do
      public? true
      allow_nil? false
    end

    attribute :is_public?, :boolean do
      public? true
      allow_nil? false
      default false
    end

    attribute :priority_level, :integer do
      public? true
      allow_nil? true
      constraints min: 1, max: 5
    end
  end
end
