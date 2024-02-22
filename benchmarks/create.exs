defmodule Resource do
  use Ash.Resource, api: Ash.Test.AnyApi

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
      Api.create!(changeset)
    end

  }
)
