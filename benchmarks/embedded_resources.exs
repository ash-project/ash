defmodule Embed do
  use Ash.Resource, data_layer: :embedded

  attributes do
    # uuid_primary_key :id
    attribute :name, :string
  end

  actions do
    defaults [:create, :update, :destroy, :read]
  end
end

defmodule Resource do
  use Ash.Resource

  attributes do
    uuid_primary_key :id
    attribute :embeds, {:array, Embed}
    attribute :maps, {:array, :map}
  end

  actions do
    defaults [:create, :update, :destroy, :read]
  end
end

defmodule Api do
  use Ash.Api

  resources do
    resource Resource
  end
end

changeset = Ash.Changeset.for_create(Resource, :create, %{})

embeds_input = 1..100 |> Enum.map(&%{name: "Embed #{&1}"})

Resource
|> Ash.Changeset.for_create(:create, %{embeds: embeds_input, maps: embeds_input})
|> Api.create!()

# :eflame.apply(fn ->
#   Resource
#   |> Ash.Changeset.for_create(:create, %{embeds: embeds_input})
#   |> Api.create!()
# end, [])

Benchee.run(
  %{
    embeds: fn ->
      Resource
      |> Ash.Changeset.for_create(:create, %{embeds: embeds_input})
      |> Api.create!()
    end,
    maps: fn ->
      Resource
      |> Ash.Changeset.for_create(:create, %{maps: embeds_input})
      |> Api.create!()
    end
  }
)
