defmodule Embed do
  use Ash.Resource, data_layer: :embedded

  attributes do
    # uuid_primary_key :id
    attribute :name, :string
  end

  actions do
    default_accept :*
    defaults [:create, :update, :destroy, :read]
  end
end

defmodule Resource do
  use Ash.Resource, domain: Domain

  attributes do
    uuid_primary_key :id
    attribute :embeds, {:array, Embed}
    attribute :maps, {:array, :map}
  end

  actions do
    default_accept :*
    defaults [:create, :update, :destroy, :read]
  end
end

defmodule Domain do
  use Ash.Domain

  resources do
    resource Resource
  end
end

embeds_input = 1..100 |> Enum.map(&%{name: "Embed #{&1}"})

Resource
|> Ash.Changeset.for_create(:create, %{embeds: embeds_input, maps: embeds_input})
|> Ash.create!()

Benchee.run(
  %{
    embeds: fn ->
      Resource
      |> Ash.Changeset.for_create(:create, %{embeds: embeds_input})
      |> Ash.create!()
    end,
    maps: fn ->
      Resource
      |> Ash.Changeset.for_create(:create, %{maps: embeds_input})
      |> Ash.create!()
    end
  }
)
