defmodule Ash.Test.Type.TimeTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.AnyApi, as: Api

  defmodule Post do
    @moduledoc false
    use Ash.Resource, api: Api, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      defaults [:create, :read, :update, :destroy]
    end

    attributes do
      uuid_primary_key :id

      attribute :time_a, :time
      attribute :time_b, :time, allow_nil?: false
    end
  end

  import Ash.Changeset

  test "it handles non-empty values" do
    post =
      Post
      |> new(%{
        time_a: ~T[08:30:00],
        time_b: ~T[15:45:30]
      })
      |> Api.create!()

    assert post.time_a == ~T[08:30:00]
    assert post.time_b == ~T[15:45:30]
  end
end
