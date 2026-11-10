# SPDX-FileCopyrightText: 2026 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Support.PolicyField.Project do
  @moduledoc false
  use Ash.Resource, domain: Ash.Test.Support.PolicyField.Domain, data_layer: Ash.DataLayer.Ets

  ets do
    private? true
  end

  attributes do
    integer_primary_key :id, writable?: true
  end

  relationships do
    has_many :tickets, Ash.Test.Support.PolicyField.ProjectTicket, public?: true
  end

  actions do
    defaults [:read, create: [:id]]
  end
end
