defmodule Ash.Test.Actions.AtomicUpdateTest do
  @moduledoc false
  use ExUnit.Case, async: true

  require Ash.Query
  require Ash.Expr

  defmodule Author do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      defaults [:create, :read, :update, :destroy]

      update :only_allow_name do
        accept([:name])
      end

      update :with_validation do
        accept([:name])

        validate attribute_equals(:name, "fred")
        validate compare(:score, greater_than_or_equal_to: 0, less_than_or_equal_to: 10)
      end

      update :increment_score do
        accept []
        change increment(:score, amount: 1, overflow_limit: 5), always_atomic?: true
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string
      attribute :bio, :string
      attribute :score, :integer
    end

    code_interface do
      define_for Ash.Test.Actions.AtomicUpdateTest.Api
      define :increment_score
    end
  end

  defmodule Api do
    @moduledoc false
    use Ash.Api

    resources do
      resource Author
    end
  end

  test "atomics can be added to a changeset" do
    author =
      Author
      |> Ash.Changeset.new(%{name: "fred"})
      |> Api.create!()

    author =
      author
      |> Ash.Changeset.for_update(:only_allow_name)
      |> Ash.Changeset.atomic_update(:name, Ash.Expr.expr(name <> " weasley"))
      |> Api.update!()

    assert author.name == "fred weasley"
  end

  test "a changeset can be fully atomic" do
    changeset =
      Ash.Changeset.fully_atomic_changeset(Author, :with_validation, %{name: "fred weasly"})

    assert changeset.valid?
    assert changeset.atomics[:name]
  end

  describe "increment/1" do
    test "it increments the value, honoring overflow" do
      author =
        Author
        |> Ash.Changeset.new(%{name: "fred", score: 0})
        |> Api.create!()

      assert Author.increment_score!(author).score == 1
      assert Author.increment_score!(author).score == 2
      assert Author.increment_score!(author).score == 3
      assert Author.increment_score!(author).score == 4
      assert Author.increment_score!(author).score == 5
      assert Author.increment_score!(author).score == 1
    end
  end
end
