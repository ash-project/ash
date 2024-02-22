defmodule Ash.Test.Resource.Changes.LoadTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.AnyApi, as: Api

  defmodule Post do
    @moduledoc false
    use Ash.Resource,
      api: Api,
      data_layer: Ash.DataLayer.Ets

    attributes do
      uuid_primary_key :id
      attribute :text, :string
      attribute :second_text, :string
    end

    actions do
      defaults [:read, :update, :destroy]

      create :create do
        change load(:full_text)
      end
    end

    calculations do
      calculate :full_text, :string, concat([:text, :second_text])
    end
  end

  test "you can use it to load on create" do
    assert Api.create!(Ash.Changeset.for_create(Post, :create, text: "foo", second_text: "bar")).full_text ==
             "foobar"
  end
end
