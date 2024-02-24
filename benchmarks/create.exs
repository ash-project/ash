defmodule Resource do
  use Ash.Resource, domain: Ash.Test.Domain

  attributes do
    uuid_primary_key :id
  end

  actions do
    defaults [:create, :update, :destroy, :read]
  end
end

changeset = Ash.Changeset.for_create(Resource, :create, %{})

Benchee.run(
  %{
    create: fn ->
      Ash.Test.Domain.create!(changeset)
    end

  }
)
