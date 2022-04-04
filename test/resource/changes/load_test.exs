defmodule Ash.Test.Resource.Changes.LoadTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule Post do
    use Ash.Resource,
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

  defmodule Registry do
    @moduledoc false
    use Ash.Registry

    entries do
      entry Post
    end
  end

  defmodule Api do
    use Ash.Api

    resources do
      registry Registry
    end
  end

  test "you can use it to load on create" do
    assert Api.create!(Ash.Changeset.for_create(Post, :create, text: "foo", second_text: "bar")).full_text ==
             "foobar"
  end
end
