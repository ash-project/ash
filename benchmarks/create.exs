defmodule Resource do
  use Ash.Resource, domain: Ash.Test.Domain

  attributes do
    uuid_primary_key :id
  end

  actions do
    defaults [:read, :destroy, create: :*, update: :*]
  end
end

changeset = Ash.Changeset.for_create(Resource, :create, %{})

Benchee.run(
  %{
    create: fn ->
      Ash.create!(changeset)
    end

  }
)
