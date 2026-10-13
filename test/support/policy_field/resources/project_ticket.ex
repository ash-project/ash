# SPDX-FileCopyrightText: 2026 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Support.PolicyField.ProjectTicket do
  @moduledoc false
  use Ash.Resource,
    domain: Ash.Test.Support.PolicyField.Domain,
    data_layer: Ash.DataLayer.Ets,
    authorizers: [Ash.Policy.Authorizer]

  ets do
    private? true
  end

  attributes do
    integer_primary_key :id, writable?: true
    attribute :hours, :integer, public?: true
    attribute :owner_id, :integer, public?: true
    attribute :project_id, :integer, public?: true
  end

  actions do
    defaults create: [:id, :hours, :owner_id, :project_id]

    read :read do
      primary? true
      pagination offset?: true, countable: true, required?: false
    end
  end

  policies do
    policy action_type(:read) do
      authorize_if always()
    end
  end

  field_policies do
    field_policy :hours do
      authorize_if expr(owner_id == ^actor(:id))
    end

    field_policy :* do
      authorize_if always()
    end
  end
end
