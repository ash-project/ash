defmodule AshKetchum.Pokemon do
  use Ash.Resource

  resource "pokemon" do
    actions do
      get true
    end

    attributes do
      attribute :name, :string
    end
  end
end
