# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Support.PolicyField.Post do
  @moduledoc false
  use Ash.Resource,
    domain: Ash.Test.Support.PolicyField.Domain,
    data_layer: Ash.DataLayer.Ets,
    authorizers: [Ash.Policy.Authorizer]

  ets do
    private? true
  end

  actions do
    default_accept :*
    defaults [:read, :destroy, create: :*, update: :*]
  end

  attributes do
    uuid_primary_key :id

    attribute :internal_status, :string do
      public?(false)
    end

    attribute :title, :string do
      public?(true)
    end

    attribute :description, :string do
      public?(true)
    end
  end

  relationships do
    belongs_to :representative, Ash.Test.Support.PolicyField.User do
      public?(true)
      allow_nil? false
    end

    belongs_to :reporter, Ash.Test.Support.PolicyField.User do
      public?(true)
      allow_nil? false
    end
  end

  policies do
    policy always() do
      authorize_if always()
    end
  end

  field_policies do
    private_fields :include

    field_policy :internal_status do
      authorize_if relates_to_actor_via(:representative)
      authorize_if relates_to_actor_via(:reporter)
    end

    field_policy :reporter_id do
      forbid_if always()
    end

    field_policy :* do
      authorize_if always()
    end
  end
end
