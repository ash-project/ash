# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
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

      update :update_if_no_bio do
        accept [:name]
        change filter(expr(is_nil(bio)))
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

  defmodule TransactionalEts do
    @moduledoc false
    # ETS, except that it supports transactions: a transaction snapshots the resource's
    # private table, and restores it when the transaction is rolled back.
    use Spark.Dsl.Extension,
      sections: Ash.DataLayer.Ets.sections(),
      transformers: Ash.DataLayer.Ets.transformers()

    @behaviour Ash.DataLayer

    callbacks = Ash.DataLayer.behaviour_info(:callbacks)

    for {name, arity} = function <- Ash.DataLayer.Ets.__info__(:functions),
        function in callbacks,
        name not in [:can?, :transaction, :rollback, :in_transaction?] do
      args = Macro.generate_arguments(arity, __MODULE__)

      @impl true
      def unquote(name)(unquote_splicing(args)) do
        Ash.DataLayer.Ets.unquote(name)(unquote_splicing(args))
      end
    end

    @impl true
    def can?(_, :transact), do: true
    def can?(resource, feature), do: Ash.DataLayer.Ets.can?(resource, feature)

    @impl true
    def in_transaction?(_), do: Process.get({__MODULE__, :in_transaction?}, false)

    @impl true
    def rollback(_, value), do: throw({__MODULE__, :rollback, value})

    @impl true
    def transaction(resource, func, _timeout, _reason) do
      table = Process.get({:ash_ets_table, Ash.DataLayer.Ets.Info.table(resource), nil})
      snapshot = table && ETS.Set.to_list!(table)
      in_transaction? = Process.put({__MODULE__, :in_transaction?}, true)

      try do
        {:ok, func.()}
      catch
        {__MODULE__, :rollback, value} ->
          if table, do: table |> ETS.Set.delete_all!() |> ETS.Set.put!(snapshot)
          {:error, value}
      after
        Process.put({__MODULE__, :in_transaction?}, in_transaction? || false)
      end
    end
  end

  defmodule FailingAfterAction do
    use Ash.Resource.Change

    def change(changeset, _, _) do
      Ash.Changeset.after_action(changeset, fn _changeset, _result ->
        {:error, "after action failed"}
      end)
    end

    def atomic(changeset, opts, context), do: {:ok, change(changeset, opts, context)}
  end

  defmodule Order do
    use Ash.Resource, domain: Domain, data_layer: TransactionalEts

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :state, :atom, allow_nil?: false, default: :open, public?: true
    end

    actions do
      defaults [:read, create: [:state]]

      update :pay do
        change set_attribute(:state, :paid)
        change FailingAfterAction
      end

      update :pay_without_transaction do
        transaction? false
        change set_attribute(:state, :paid)
        change FailingAfterAction
      end
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

  test "filters added by an action's changes are applied when upgraded to an atomic update" do
    author =
      Author
      |> Ash.Changeset.for_create(:create, %{name: "fred", bio: "a bio"})
      |> Ash.create!()

    assert {:error, %Ash.Error.Invalid{errors: [%Ash.Error.Changes.StaleRecord{}]}} =
             author
             |> Ash.Changeset.for_update(:update_if_no_bio, %{name: "george"})
             |> Ash.update()

    assert Ash.get!(Author, author.id).name == "fred"
  end

  test "filters added by an action's changes and by the caller are both applied when upgraded to an atomic update" do
    author =
      Author
      |> Ash.Changeset.for_create(:create, %{name: "fred"})
      |> Ash.create!()

    assert {:error, %Ash.Error.Invalid{errors: [%Ash.Error.Changes.StaleRecord{}]}} =
             author
             |> Ash.Changeset.new()
             |> Ash.Changeset.filter(Ash.Expr.expr(name == "george"))
             |> Ash.Changeset.for_update(:update_if_no_bio, %{name: "george"})
             |> Ash.update()

    assert %{name: "george"} =
             author
             |> Ash.Changeset.new()
             |> Ash.Changeset.filter(Ash.Expr.expr(name == "fred"))
             |> Ash.Changeset.for_update(:update_if_no_bio, %{name: "george"})
             |> Ash.update!()
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

  describe "update_timestamp with nullable atomic changes" do
    test "update_timestamp is updated when atomically changing a nullable attribute from nil to a value" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "fred"})
        |> Ash.create!()

      assert is_nil(author.score)
      original_updated_at = author.updated_at

      [author] =
        Author
        |> Ash.Query.filter(id == ^author.id)
        |> Ash.bulk_update!(:update, %{score: 10},
          return_records?: true,
          strategy: :atomic
        )
        |> Map.get(:records)

      assert author.score == 10
      assert DateTime.compare(author.updated_at, original_updated_at) != :eq
    end

    test "update_timestamp is updated when atomically changing a nullable attribute from a value to nil" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "fred", score: 5})
        |> Ash.create!()

      original_updated_at = author.updated_at

      [author] =
        Author
        |> Ash.Query.filter(id == ^author.id)
        |> Ash.bulk_update!(:update, %{score: nil},
          return_records?: true,
          strategy: :atomic
        )
        |> Map.get(:records)

      assert is_nil(author.score)
      assert DateTime.compare(author.updated_at, original_updated_at) != :eq
    end

    test "update_timestamp is not updated when atomically setting a nullable attribute to its current nil value" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "fred"})
        |> Ash.create!()

      assert is_nil(author.score)
      original_updated_at = author.updated_at

      [author] =
        Author
        |> Ash.Query.filter(id == ^author.id)
        |> Ash.bulk_update!(:update, %{score: nil},
          return_records?: true,
          strategy: :atomic
        )
        |> Map.get(:records)

      assert is_nil(author.score)
      assert DateTime.compare(author.updated_at, original_updated_at) == :eq
    end
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

  describe "after_action errors when upgraded to an atomic update" do
    test "roll back the update" do
      order = Ash.create!(Order, %{})

      assert {:error, _} =
               order
               |> Ash.Changeset.for_update(:pay)
               |> Ash.update()

      assert Ash.get!(Order, order.id).state == :open
    end

    test "do not roll back the update with `rollback_on_error?: false`" do
      order = Ash.create!(Order, %{})

      assert {:error, _} =
               order
               |> Ash.Changeset.for_update(:pay)
               |> Ash.update(rollback_on_error?: false)

      assert Ash.get!(Order, order.id).state == :paid
    end

    test "do not roll back the update when the action has `transaction? false`" do
      order = Ash.create!(Order, %{})

      assert {:error, _} =
               order
               |> Ash.Changeset.for_update(:pay_without_transaction)
               |> Ash.update()

      assert Ash.get!(Order, order.id).state == :paid
    end
  end
end
