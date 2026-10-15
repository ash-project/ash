# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Actions.AtomicChangeErrorTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule ErrorInAtomic do
    @moduledoc false
    use Ash.Resource.Change

    @impl true
    def atomic(_changeset, _opts, _context) do
      {:error, Ash.Error.Changes.InvalidAttribute.exception(field: :count, message: "nope")}
    end
  end

  defmodule UncastableAtomic do
    @moduledoc false
    use Ash.Resource.Change

    @impl true
    def atomic(_changeset, _opts, _context), do: {:atomic, %{count: "not a number"}}
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :count, :integer, default: 0, public?: true
    end

    actions do
      defaults [:read, create: [:count]]

      create :create_with_error do
        change ErrorInAtomic
      end

      update :bump do
        change ErrorInAtomic
        change atomic_update(:count, expr(count + 1))
      end

      update :set_uncastable do
        change UncastableAtomic
      end

      destroy :destroy do
        primary? true
        change ErrorInAtomic
      end
    end
  end

  defp assert_nope(errors) do
    assert Enum.any?(List.wrap(errors), fn error ->
             error
             |> Ash.Error.to_error_class()
             |> Map.get(:errors)
             |> Enum.any?(&match?(%Ash.Error.Changes.InvalidAttribute{message: "nope"}, &1))
           end)
  end

  test "fully_atomic_changeset returns an invalid changeset carrying the error" do
    assert %Ash.Changeset{valid?: false, errors: errors} =
             Ash.Changeset.fully_atomic_changeset(Post, :bump, %{})

    assert_nope(errors)
  end

  test "a bulk update by query fails with the error" do
    Ash.create!(Post, %{})

    assert %Ash.BulkResult{status: :error, errors: errors} =
             Ash.bulk_update(Post, :bump, %{}, strategy: [:atomic], return_errors?: true)

    assert_nope(errors)
    assert [%{count: 0}] = Ash.read!(Post)
  end

  test "a bulk update with the stream strategy fails with the error" do
    Ash.create!(Post, %{})

    assert %Ash.BulkResult{status: :error, errors: errors} =
             Ash.bulk_update(Post, :bump, %{}, strategy: [:stream], return_errors?: true)

    assert_nope(errors)
    assert [%{count: 0}] = Ash.read!(Post)
  end

  test "a single atomic update of a loaded record fails with the error" do
    post = Ash.create!(Post, %{})

    assert {:error, error} = post |> Ash.Changeset.for_update(:bump) |> Ash.update()

    assert_nope(error)
    assert [%{count: 0}] = Ash.read!(Post)
  end

  test "a bulk destroy by query fails with the error" do
    Ash.create!(Post, %{})

    assert %Ash.BulkResult{status: :error, errors: errors} =
             Ash.bulk_destroy(Post, :destroy, %{}, strategy: [:atomic], return_errors?: true)

    assert_nope(errors)
    assert [_] = Ash.read!(Post)
  end

  test "a bulk create fails with the error" do
    assert %Ash.BulkResult{status: :error, errors: errors} =
             Ash.bulk_create([%{}], Post, :create_with_error, return_errors?: true)

    assert_nope(errors)
    assert [] = Ash.read!(Post)
  end

  describe "an atomic value that fails to cast" do
    defp assert_invalid_count(errors) do
      assert Enum.any?(List.wrap(errors), fn error ->
               error
               |> Ash.Error.to_error_class()
               |> Map.get(:errors)
               |> Enum.any?(&match?(%Ash.Error.Changes.InvalidAttribute{field: :count}, &1))
             end)
    end

    test "fails a bulk update by query with an invalid attribute error" do
      Ash.create!(Post, %{})

      assert %Ash.BulkResult{status: :error, errors: errors} =
               Ash.bulk_update(Post, :set_uncastable, %{},
                 strategy: [:atomic],
                 return_errors?: true
               )

      assert_invalid_count(errors)
      assert [%{count: 0}] = Ash.read!(Post)
    end

    test "fails a single update with an invalid attribute error" do
      post = Ash.create!(Post, %{})

      assert {:error, error} =
               post |> Ash.Changeset.for_update(:set_uncastable) |> Ash.update()

      assert_invalid_count(error)
      assert [%{count: 0}] = Ash.read!(Post)
    end
  end
end
