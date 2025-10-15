# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Domain do
  use Ash.Domain, validate_config_inclusion?: false

  resources do
    allow_unregistered? true
  end
end

defmodule Embed do
  use Ash.Resource, data_layer: :embedded

  attributes do
    # uuid_primary_key :id
    attribute :name, :string, public?: true
  end

  actions do
    default_accept :*
    defaults [:read, :destroy, create: :*, update: :*]
  end

  changes do
    change fn changeset, _ -> changeset end
  end
end

defmodule Resource do
  use Ash.Resource, domain: Domain

  attributes do
    uuid_primary_key :id
    attribute :embeds, {:array, Embed}, public?: true

    attribute :structs, {:array, :struct} do
      public? true

      constraints items: [
                    instance_of: Embed,
                    fields: [
                      name: [
                        type: :string
                      ]
                    ]
                  ]
    end

    attribute :maps, {:array, :map}, public?: true
  end

  actions do
    default_accept :*
    defaults [:read, :destroy, create: :*, update: :*]
  end
end

embeds_input = 1..100 |> Enum.map(&%{name: "Embed #{&1}"})

resource_embeds_input =
  1..100
  |> Enum.map(fn _ -> %{embeds: embeds_input} end)

resource_maps_input =
  1..100
  |> Enum.map(fn _ -> %{maps: embeds_input} end)

resource_structs_input =
  1..100
  |> Enum.map(fn _ -> %{structs: embeds_input} end)

Resource
|> Ash.Changeset.for_create(:create, %{embeds: embeds_input, maps: embeds_input})
|> Ash.create!()

Benchee.run(
  %{
    embeds: fn ->
      Ash.bulk_create!(resource_embeds_input, Resource, :create)
    end,
    maps: fn ->
      Ash.bulk_create!(resource_maps_input, Resource, :create)
    end,
    structs: fn ->
      Ash.bulk_create!(resource_structs_input, Resource, :create)
    end
  },
  memory_time: 2
)
