# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.InputParsing.HistoryEntry do
  @moduledoc """
  Embedded resource for testing arrays of embedded resources.

  Used to verify that arrays of embedded resources correctly apply
  how embedded-resource arguments appear in the manifest.
  """
  use Ash.Resource,
    data_layer: :embedded

  attributes do
    attribute :action_name, :string do
      allow_nil? false
      public? true
    end

    attribute :timestamp, :utc_datetime do
      allow_nil? false
      public? true
    end

    attribute :change_count_1, :integer do
      default 1
      public? true
    end

    attribute :was_reverted?, :boolean do
      default false
      public? true
    end

    attribute :details, :map do
      allow_nil? true
      public? true
    end
  end

  actions do
    defaults [:read, :destroy, create: :*, update: :*]
  end
end
