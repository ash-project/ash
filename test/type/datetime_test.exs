# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Type.DateTimeTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule Post do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    attributes do
      uuid_primary_key :id

      attribute :utc_datetime, :utc_datetime do
        public?(true)
      end

      attribute :utc_datetime_no_dates, :utc_datetime do
        constraints cast_dates_as: :error
        public?(true)
      end
    end
  end

  test "it allows dates by default" do
    today = Date.utc_today()

    post =
      Post
      |> Ash.Changeset.for_create(:create, %{
        utc_datetime: today
      })
      |> Ash.create!()

    assert DateTime.to_date(post.utc_datetime) == today
  end

  test "it does not allow dates if `cast_dates_as` is `:error`" do
    today = Date.utc_today()

    assert_raise Ash.Error.Invalid, ~r"must be a datetime, got a date", fn ->
      Post
      |> Ash.Changeset.for_create(:create, %{
        utc_datetime_no_dates: today
      })
      |> Ash.create!()
    end
  end
end
