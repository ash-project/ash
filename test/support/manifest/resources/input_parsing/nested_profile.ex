# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.InputParsing.NestedProfile do
  @moduledoc """
  Embedded resource containing another embedded resource for deep nesting tests.

  Tests 3-level nesting: Resource → NestedProfile → Profile (embedded)
  Each level has its own constrained fields.
  """
  use Ash.Resource,
    data_layer: :embedded

  attributes do
    attribute :section_name, :string do
      allow_nil? false
      public? true
    end

    attribute :level_1?, :boolean do
      default false
      public? true
    end

    attribute :has_details?, :boolean do
      default true
      public? true
    end

    # Embedded resource within embedded resource - creates 3-level nesting
    attribute :detail_profile, Ash.Test.Manifest.InputParsing.Profile do
      allow_nil? true
      public? true
    end

    # NewType with constrained fields nested in an embedded resource
    attribute :detail_stats, Ash.Test.Manifest.InputParsing.Stats do
      allow_nil? true
      public? true
    end
  end

  actions do
    defaults [:read, :destroy, create: :*, update: :*]
  end
end
