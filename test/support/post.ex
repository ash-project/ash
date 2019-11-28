defmodule Ash.Test.Post do
  use Ash.Resource, name: "posts", type: "post"
  use Ash.DataLayer.Ets, private: true

  actions do
    defaults [:read, :create]
  end

  attributes do
    attribute :title, :string
    attribute :contents, :string
  end
end
