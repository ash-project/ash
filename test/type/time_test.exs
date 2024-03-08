defmodule Ash.Test.Type.TimeTest do
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
      defaults [:create, :read, :update, :destroy]
    end

    attributes do
      uuid_primary_key :id

      attribute :time_a, :time do
        public?(true)
      end

      attribute :time_b, :time, allow_nil?: false, public?: true
    end
  end

  test "it handles non-empty values" do
    post =
      Post
      |> Ash.Changeset.for_create(:create, %{
        time_a: ~T[08:30:00],
        time_b: ~T[15:45:30]
      })
      |> Ash.create!()

    assert post.time_a == ~T[08:30:00]
    assert post.time_b == ~T[15:45:30]
  end
end
