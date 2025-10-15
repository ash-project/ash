# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Domain do
  use Ash.Domain, validate_config_inclusion?: false

  resources do
    allow_unregistered? true
  end
end

defmodule Destination do
  use Ash.Resource,
    data_layer: Ash.DataLayer.Ets,
    domain: Domain

  actions do
    defaults [:read, :destroy, create: :*, update: :*]
  end

  attributes do
    uuid_primary_key :id
    attribute :name, :string, allow_nil?: false, public?: true
  end

  relationships do
    belongs_to :source, Source, public?: true
  end
end

defmodule Source do
  use Ash.Resource,
    data_layer: Ash.DataLayer.Ets,
    domain: Domain

  actions do
    defaults [:read, :destroy, create: :*, update: :*]
  end

  attributes do
    uuid_primary_key :id
    attribute :first_name, :string, allow_nil?: false, public?: true
    attribute :last_name, :string, allow_nil?: false, public?: true
  end

  calculations do
    calculate :full_name, :string, expr(first_name <> " " <> last_name)
  end

  aggregates do
    first :first_destination_name, :destination, :name
  end

  relationships do
    has_many :destination, Destination
  end
end

source =
  Source
  |> Ash.Changeset.for_create(:create, %{first_name: "John", last_name: "Doe"})
  |> Ash.create!()

for _ <- 1..2 do
  Destination
  |> Ash.Changeset.for_create(:create, %{source_id: source.id, name: "Destination"})
  |> Ash.create!()
end

query =
  Source
  |> Ash.Query.for_read(:read, %{})
  |> Ash.Query.load([:first_destination_name, :full_name, :destination])

Ash.read!(query)

Logger.configure(level: :error)

:eflame.apply(
  fn ->
    Ash.read!(query)
  end,
  []
)
