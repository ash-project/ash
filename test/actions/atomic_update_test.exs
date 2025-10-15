# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

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

  # Forces `where` conditions to be evaluated atomically.
  defmodule AtomicOnlyValidation do
    use Ash.Resource.Validation

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
      defaults [:read, :destroy, create: :*, update: :*]

      update :only_allow_name do
        accept([:name])
      end

      update :with_validation do
        accept([:name])

        validate attribute_equals(:name, "fred")
        validate compare(:score, greater_than_or_equal_to: 0, less_than_or_equal_to: 10)
      end

      update :with_conditional_validation do
        accept([:bio, :name, :score])

        validate present(:bio) do
          where [
            AtomicOnlyValidation,
            attribute_equals(:name, "Bill S. Preston, Esq."),
            changing(:score),
            compare(:score, greater_than_or_equal_to: 0, less_than_or_equal_to: 10)
          ]

          message "If your name is Bill S. Preston, Esq. and you're providing a new score between 0 and 10, you most provide a bio!"
        end
      end

      update :with_around_action do
        require_atomic? false

        change fn changeset, _ ->
          Ash.Changeset.around_action(changeset, fn changeset, callback ->
            raise "Around action!"
          end)
        end
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

      update :relate_actor do
        accept []
        change relate_actor(:owner)
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

      update_timestamp :updated_at
    end

    relationships do
      belongs_to :owner, __MODULE__
    end

    code_interface do
      define :increment_score
      define :sometimes_atomic
    end
  end

  # Resource without update_timestamp attribute
  defmodule Book do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    resource do
      atomic_validation_default_target_attribute :title
    end

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]

      update :validation_with_update do
        validate compare(:year, greater_than_or_equal_to: 2000)
        change atomic_update(:year, expr(year + 1))
      end

      update :validation_without_update do
        accept [:title, :year]
        validate compare(:year, greater_than_or_equal_to: 2000)
      end
    end

    attributes do
      uuid_primary_key :id

      attribute :title, :string do
        public?(true)
      end

      attribute :year, :integer do
        public?(true)
      end
    end

    code_interface do
      define :validation_with_update
      define :validation_without_update
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
      Ash.Changeset.fully_atomic_changeset(Author, :with_validation, %{name: "fred"},
        eager?: false
      )

    assert changeset.valid?
  end

  test "a changes" do
    author =
      Author
      |> Ash.Changeset.for_create(:create, %{name: "fred", score: 0})
      |> Ash.create!()

    assert_raise Ash.Error.Unknown, ~r/Around action/, fn ->
      Author
      |> Ash.Query.filter(id == ^author.id)
      |> Ash.bulk_update!(:with_around_action, %{name: "george"},
        return_errors?: true,
        strategy: :stream
      )
    end
  end

  test "manage_relationship in a change is discarded" do
    maker =
      Author
      |> Ash.Changeset.for_create(:create, %{name: "fred", score: 0})
      |> Ash.create!()

    author =
      Author
      |> Ash.Changeset.for_create(:create, %{name: "fred", score: 0})
      |> Ash.create!()

    author
    |> Ash.Changeset.for_update(:relate_actor, %{}, actor: maker)
    |> Ash.update!()
  end

  test "values are eagerly validated" do
    changeset =
      Ash.Changeset.fully_atomic_changeset(Author, :with_validation, %{name: "fred weasly"})

    refute changeset.valid?
  end

  test "uses changing attribute for validation in changeset" do
    changeset =
      Ash.Changeset.fully_atomic_changeset(Book, :validation_with_update, %{})

    # Uses the *changing* attribute for the validation expression.
    assert Keyword.keys(changeset.atomics) == [:year]
  end

  test "using changing attribute for validation works" do
    recent_book =
      Book
      |> Ash.Changeset.for_create(:create, %{title: "RecentBook", year: 2020})
      |> Ash.create!()

    older_book =
      Book
      |> Ash.Changeset.for_create(:create, %{title: "OlderBook", year: 1960})
      |> Ash.create!()

    # Validation passes for recent books, fails for older ones.
    assert {:ok, %Book{year: 2021}} = Book.validation_with_update(recent_book)
    assert {:error, %Ash.Error.Invalid{}} = Book.validation_with_update(older_book)
  end

  test "uses default target attribute for validation in changeset" do
    changeset =
      Ash.Changeset.fully_atomic_changeset(Book, :validation_without_update, %{})

    # Uses the default atomic attribute for the validation expression.
    assert Keyword.keys(changeset.atomics) == [:title]
  end

  test "using default target attribute for validation works" do
    recent_book =
      Book
      |> Ash.Changeset.for_create(:create, %{title: "RecentBook", year: 2020})
      |> Ash.create!()

    older_book =
      Book
      |> Ash.Changeset.for_create(:create, %{title: "OlderBook", year: 1960})
      |> Ash.create!()

    # Validation passes for recent books, fails for older ones.
    assert {:ok, %Book{year: 2020}} = Book.validation_without_update(recent_book)
    assert {:error, %Ash.Error.Invalid{}} = Book.validation_without_update(older_book)
  end

  test "can update validation default target attribute" do
    recent_book =
      Book
      |> Ash.Changeset.for_create(:create, %{title: "RecentBook", year: 2020})
      |> Ash.create!()

    updated_book = Book.validation_without_update!(recent_book, %{title: "UpdatedTitle"})

    assert updated_book.title == "UpdatedTitle"
  end

  test "policies that require original data" do
    author =
      Author
      |> Ash.Changeset.for_create(:create, %{name: "fred", score: 0})
      |> Ash.create!()

    assert Author.increment_score!(author, authorize?: true).score == 1
  end

  test "validations are properly negated when used as `where` conditions" do
    with_conditional_validation_fn = fn attrs ->
      Author
      |> Ash.Changeset.for_create(:create, %{name: "Ted Theodore Logan", score: 0})
      |> Ash.create!()
      |> Ash.Changeset.for_update(:with_conditional_validation, attrs)
      |> Ash.update()
    end

    trigger_condition_attrs = %{name: "Bill S. Preston, Esq.", score: 5}

    # Validation to check bio is triggered due to `where` conditions.
    assert {:error, %{errors: [%{message: msg}]}} =
             with_conditional_validation_fn.(trigger_condition_attrs)

    assert msg =~ ~r/you most provide a bio!\Z/

    # Bio is present and validations passed.
    bio = "Founding member of the Wyld Stallyns."

    assert {:ok, %{bio: ^bio, name: "Bill S. Preston, Esq.", score: 5}} =
             trigger_condition_attrs
             |> Map.put(:bio, bio)
             |> with_conditional_validation_fn.()

    # Score is too high and conditions aren't met so bio isn't required.
    assert {:ok, %{bio: nil, name: "Bill S. Preston, Esq.", score: 9_000}} =
             trigger_condition_attrs
             |> Map.put(:score, 9_000)
             |> with_conditional_validation_fn.()
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
