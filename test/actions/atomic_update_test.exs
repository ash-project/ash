defmodule Ash.Test.Actions.AtomicUpdateTest do
  @moduledoc false
  use ExUnit.Case, async: true

  require Ash.Query
  require Ash.Expr

  alias Ash.Test.Domain, as: Domain

  defmodule Atomic do
    use Ash.Resource.Change

    def atomic(_, _, _) do
      :ok
    end
  end

  defmodule NotAtomic do
    use Ash.Resource.Change

    def change(changeset, _, _) do
      changeset
    end
  end

  defmodule Author do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
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

      update :sometimes_atomic do
        accept []
        require_atomic? true
        change Atomic
      end
    end

    attributes do
      uuid_primary_key :id

      attribute :name, :string do
        public?(true)
      end

      attribute :bio, :string do
        public?(true)
      end

      attribute :score, :integer do
        public?(true)
      end
    end

    code_interface do
      define :increment_score
      define :sometimes_atomic
    end
  end

  test "atomics can be added to a changeset" do
    author =
      Author
      |> Ash.Changeset.for_create(:create, %{name: "fred"})
      |> Ash.create!()

    author =
      author
      |> Ash.Changeset.for_update(:only_allow_name)
      |> Ash.Changeset.atomic_update(:name, Ash.Expr.expr(name <> " weasley"))
      |> Ash.update!()

    assert author.name == "fred weasley"
  end

  test "a changeset can be fully atomic" do
    changeset =
      Ash.Changeset.fully_atomic_changeset(Author, :with_validation, %{name: "fred weasly"},
        eager?: false
      )

    assert changeset.valid?
  end

  test "values are eagerly validated" do
    changeset =
      Ash.Changeset.fully_atomic_changeset(Author, :with_validation, %{name: "fred weasly"})

    refute changeset.valid?
  end

  test "policies that require original data" do
    author =
      Author
      |> Ash.Changeset.for_create(:create, %{name: "fred", score: 0})
      |> Ash.create!()

    assert Author.increment_score!(author, authorize?: true).score == 1
  end

  describe "increment/1" do
    test "it increments the value, honoring overflow" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "fred", score: 0})
        |> Ash.create!()

      assert Author.increment_score!(author).score == 1
      assert Author.increment_score!(author).score == 2
      assert Author.increment_score!(author).score == 3
      assert Author.increment_score!(author).score == 4
      assert Author.increment_score!(author).score == 5
      assert Author.increment_score!(author).score == 1
    end
  end
end
