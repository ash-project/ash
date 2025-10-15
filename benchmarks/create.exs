# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Domain do
  use Ash.Domain, validate_config_inclusion?: false

  resources do
    allow_unregistered? true
  end
end

defmodule Resource do
  use Ash.Resource, domain: Domain

  attributes do
    uuid_primary_key :id
  end

  actions do
    defaults [:read, :destroy, create: :*, update: :*]
  end
end

changeset = Ash.Changeset.for_create(Resource, :create, %{})

Benchee.run(%{
  create: fn ->
    Ash.create!(changeset)
  end
})
