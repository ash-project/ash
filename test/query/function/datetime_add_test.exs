# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Function.DateTimeAddTest do
  @moduledoc false
  use ExUnit.Case, async: true

  import Ash.Expr
  require Ash.Query

  alias Ash.Test.Domain, as: Domain

  defmodule Event do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key(:id)
      attribute(:happened_at, :naive_datetime, public?: true)
      attribute(:recorded_at, :utc_datetime_usec, public?: true)
    end

    actions do
      default_accept(:*)
      defaults([:read, :create])
    end
  end

  setup do
    Event
    |> Ash.Changeset.for_create(:create, %{
      happened_at: ~N[2024-01-31 12:00:00],
      recorded_at: ~U[2024-01-31 12:00:00.123456Z]
    })
    |> Ash.create!()

    :ok
  end

  test "accepts a utc_datetime_usec reference" do
    assert [_] =
             Event
             |> Ash.Query.filter(datetime_add(recorded_at, 1, :day) > ^~U[2024-02-01 00:00:00Z])
             |> Ash.read!()
  end

  test "accepts a naive_datetime reference with an interval" do
    assert [_] =
             Event
             |> Ash.Query.filter(datetime_add(happened_at, 1, :month) == ^~N[2024-02-29 12:00:00])
             |> Ash.read!()

    assert [] =
             Event
             |> Ash.Query.filter(datetime_add(happened_at, 1, :hour) > ^~N[2024-01-31 14:00:00])
             |> Ash.read!()
  end

  test "accepts a naive_datetime reference with a duration" do
    assert [_] =
             Event
             |> Ash.Query.filter(
               datetime_add(happened_at, ^Duration.new!(day: 1)) == ^~N[2024-02-01 12:00:00]
             )
             |> Ash.read!()
  end

  test "evaluates naive datetimes directly" do
    assert {:ok, ~N[2024-01-31 12:00:30]} =
             Ash.Expr.eval(expr(datetime_add(^~N[2024-01-31 12:00:00], 30, :second)))

    assert {:ok, ~N[2024-02-07 12:00:00]} =
             Ash.Expr.eval(expr(datetime_add(^~N[2024-01-31 12:00:00], ^Duration.new!(week: 1))))
  end
end
