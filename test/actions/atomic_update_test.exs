defmodule Ash.Test.Actions.AtomicUpdateTest do
  @moduledoc false
  use ExUnit.Case, async: true

  require Ash.Query
  require Ash.Expr

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

      update :sometimes_atomic do
        accept []
        require_atomic? true
        change Atomic
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
      define :sometimes_atomic
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
      Ash.Changeset.fully_atomic_changeset(Author, :with_validation, %{name: "fred weasly"},
        eager?: false
      )

    assert changeset.valid?
    assert changeset.atomics[:name]
  end

  test "values are eagerly validated" do
    changeset =
      Ash.Changeset.fully_atomic_changeset(Author, :with_validation, %{name: "fred weasly"})

    refute changeset.valid?
  end

  test "an action that cannot be done fully atomically raises an error at runtime" do
    author =
      Author
      |> Ash.Changeset.new(%{name: "fred", score: 0})
      |> Api.create!()

    assert_raise Ash.Error.Framework, ~r/must be performed atomically/, fn ->
      Author.sometimes_atomic!(author, %{atomic: false})
    end
  end

  # See the note in `Ash.Resource.Verifiers.VerifyActionsAtomic` for why we can't introduce
  # this yet.
  # test "resource compilation fails if an action is known to not be able to be made atomic" do
  #   assert_raise Spark.Error.DslError, ~r/can never be done atomically/, fn ->
  #     defmodule ExampleNonAtomic do
  #       use Ash.Resource

  #       attributes do
  #         uuid_primary_key :id
  #       end

  #       actions do
  #         update :update do
  #           require_atomic? true
  #           change NotAtomic
  #         end
  #       end
  #     end
  #   end
  # end

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
