defmodule Ash.Test.Type.NaiveDateTimeTest do
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
      defaults [:create, :read, :update, :destroy]
    end

    attributes do
      uuid_primary_key :id

      attribute :naive_datetime_a, :naive_datetime
      attribute :naive_datetime_b, :naive_datetime, allow_nil?: false
    end
  end

  import Ash.Changeset

  test "it handles non-empty values" do
    post =
      Post
      |> new(%{
        naive_datetime_a: ~N[2022-04-17 08:30:00],
        naive_datetime_b: ~N[2022-04-17 15:45:30]
      })
      |> Domain.create!()

    assert post.naive_datetime_a == ~N[2022-04-17 08:30:00]
    assert post.naive_datetime_b == ~N[2022-04-17 15:45:30]
  end
end
