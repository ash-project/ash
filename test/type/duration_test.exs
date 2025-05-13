defmodule Ash.Test.Type.DurationTest do
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

      attribute :duration_a, :duration do
        public?(true)
      end

      attribute :duration_b, :duration, allow_nil?: false, public?: true
    end
  end

  test "it handles non-empty values" do
    post =
      Post
      |> Ash.Changeset.for_create(:create, %{
        duration_a: Duration.new!(hour: 1),
        duration_b: Duration.new!(minute: 30)
      })
      |> Ash.create!()

    assert post.duration_a == %Duration{hour: 1}
    assert post.duration_b == %Duration{minute: 30}
  end

  test "comparable protocol" do
    hour1 = Duration.new!(hour: 1)
    min60 = Duration.new!(minute: 60)
    min30 = Duration.new!(minute: 30)
    assert Comp.compare(hour1, min60) == :eq
    assert Comp.compare(hour1, min30) == :gt
    assert Comp.compare(min30, min60) == :lt
  end
end
