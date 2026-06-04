# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Actions.UpdateAllTest do
  @moduledoc """
  Tests for `Ash.update_all/3` — the record-by-record bulk update (distinct input per record),
  including `transaction: :per_record`.

  Note: the ETS data layer does not support transactions, so these tests verify the *result shape*
  (distinct per-record inputs, per-input index correlation, partial success) rather than
  database-level rollback isolation, which is exercised against AshPostgres.
  """
  use ExUnit.Case, async: false

  alias Ash.Test.Domain, as: Domain

  defmodule RejectBadTitle do
    @moduledoc false
    use Ash.Resource.Validation

    @impl true
    def validate(changeset, _opts, _context) do
      case Ash.Changeset.get_attribute(changeset, :title) do
        "bad" -> {:error, field: :title, message: "title cannot be bad"}
        _ -> :ok
      end
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*]

      update :update do
        primary? true
        require_atomic? false
        accept [:title, :status]
        validate RejectBadTitle
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, allow_nil?: false, public?: true
      attribute :status, :string, public?: true
    end
  end

  defp create_post(title) do
    Post
    |> Ash.Changeset.for_create(:create, %{title: title})
    |> Ash.create!()
  end

  defp reload(post), do: Ash.get!(Post, post.id)

  test "applies a distinct input to each record and correlates results by input index" do
    p1 = create_post("one")
    p2 = create_post("two")
    p3 = create_post("three")

    result =
      Ash.update_all(
        [
          {p1, %{title: "one_updated"}},
          {p2, %{title: "two_updated"}},
          {p3, %{title: "three_updated"}}
        ],
        :update,
        resource: Post,
        transaction: :per_record,
        stop_on_error?: false,
        return_records?: true,
        return_errors?: true,
        sorted?: true
      )

    assert %Ash.BulkResult{status: :success, error_count: 0, records: records} = result
    assert Enum.map(records, & &1.title) == ["one_updated", "two_updated", "three_updated"]
    assert Enum.map(records, & &1.__metadata__.bulk_update_index) == [0, 1, 2]

    assert reload(p1).title == "one_updated"
    assert reload(p2).title == "two_updated"
    assert reload(p3).title == "three_updated"
  end

  test "one failing record does not fail the others (partial success, indexed error)" do
    p1 = create_post("keep1")
    p2 = create_post("keep2")
    p3 = create_post("keep3")

    result =
      Ash.update_all(
        [
          {p1, %{title: "ok1"}},
          {p2, %{title: "bad"}},
          {p3, %{title: "ok3"}}
        ],
        :update,
        resource: Post,
        transaction: :per_record,
        stop_on_error?: false,
        return_records?: true,
        return_errors?: true,
        sorted?: true
      )

    assert %Ash.BulkResult{
             status: :partial_success,
             error_count: 1,
             records: records,
             errors: [error]
           } = result

    assert Enum.map(records, & &1.title) == ["ok1", "ok3"]
    assert Enum.map(records, & &1.__metadata__.bulk_update_index) == [0, 2]
    assert reload(p1).title == "ok1"
    assert reload(p3).title == "ok3"

    assert reload(p2).title == "keep2"
    assert [1 | _] = error.path
  end

  test "empty input short-circuits to a success result" do
    result = Ash.update_all([], :update, resource: Post, return_records?: true)

    assert %Ash.BulkResult{status: :success, records: [], error_count: 0} = result
  end

  test "derives the resource from the records when no :resource option is given" do
    p1 = create_post("derive_me")

    result =
      Ash.update_all([{p1, %{title: "derived"}}], :update,
        transaction: :per_record,
        return_records?: true
      )

    assert %Ash.BulkResult{status: :success, records: [record]} = result
    assert record.title == "derived"
    assert reload(p1).title == "derived"
  end
end
