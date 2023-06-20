defmodule Post do
  use Ash.Resource,
    data_layer: Ash.DataLayer.Ets

  ets do
    private? true
  end

  actions do
    defaults [:create, :read, :update, :destroy]
  end

  attributes do
    uuid_primary_key :id
  end
end

defmodule Api do
  use Ash.Api

  resources do
    allow_unregistered? true
  end
end

Benchee.run(
  %{
    old: fn changeset ->
      Api.create!(changeset)
    end,
    new: fn changeset ->
      Reactor.run(Ash.Actions.Create.CreateReactor, %{changeset: changeset, opts: []})
    end
  },
  inputs: %{
    "changeset" => Post
      |> Ash.Changeset.for_create(:create, %{})
      |> Map.put(:api, Api)
  }
)
