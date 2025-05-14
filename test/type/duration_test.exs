defmodule Ash.Test.Type.DurationTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  import Ash.Expr
  alias Ash.Query.Operator.Basic

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

    calculations do
      calculate :duration_a_plus_b, :duration, expr(duration_a + duration_b)
      calculate :duration_b_minus_a, :duration, expr(duration_b - duration_a)
      #calculate :two_times_duration_a, :duration, expr(2 * duration_a)
      #calculate :duration_b_times_three, :duration, expr(duration_b * 3)
      calculate :duration_a_negated, :duration, expr(-duration_a)
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

  describe "functions" do
    test "minus function performs negation" do
      year1 = Duration.new!(year: 1)
      assert Ash.Query.Function.Minus.evaluate(%{arguments: [year1]}) == {:known, Duration.new!(year: -1)}
    end
  end

  describe "operators" do
    test "plus operator performs addition" do
      year1 = Duration.new!(year: 1)
      month5 = Duration.new!(month: 5)
      assert Basic.Plus.evaluate(%{left: year1, right: month5}) == {:known, Duration.add(year1, month5)}
    end

    test "minus operator performs subtraction" do
      year1 = Duration.new!(year: 1)
      month5 = Duration.new!(month: 5)
      assert Basic.Minus.evaluate(%{left: year1, right: month5}) == {:known, Duration.subtract(year1, month5)}
    end

    test "times operator performs duration times integer" do
      year1 = Duration.new!(year: 1)
      assert Basic.Times.evaluate(%{left: year1, right: 2}) == {:known, Duration.multiply(year1, 2)}
    end

    test "times operator performs integer times duration" do
      year1 = Duration.new!(year: 1)
      assert Basic.Times.evaluate(%{left: 2, right: year1}) == {:known, Duration.multiply(year1, 2)}
    end
  end

  test "comparable protocol" do
    year1 = Duration.new!(year: 1)
    month12 = Duration.new!(month: 12)
    week1 = Duration.new!(week: 1)
    day7 = Duration.new!(day: 7)
    hour1 = Duration.new!(hour: 1)
    min60 = Duration.new!(minute: 60)
    min30 = Duration.new!(minute: 30)
    assert Comp.compare(hour1, min30) == :gt
    assert Comp.compare(min30, min60) == :lt
    assert Comp.compare(hour1, min60) == :eq
    assert Comp.compare(week1, day7) == :eq
    assert Comp.compare(year1, month12) == :eq
  end

  test "calculations" do
    post =
      Post
      |> Ash.Changeset.for_create(:create, %{
        duration_a: Duration.new!(hour: 1),
        duration_b: Duration.new!(minute: 30)
      })
      |> Ash.create!()
      #|> Ash.load!([:duration_a_plus_b, :duration_b_minus_a, :two_times_duration_a, :duration_b_times_three, :duration_a_negated])
      |> Ash.load!([:duration_a_plus_b, :duration_b_minus_a, :duration_a_negated])

    assert Comp.compare(post.duration_a_plus_b, %Duration{hour: 1, minute: 30}) == :eq
    assert Comp.compare(post.duration_b_minus_a, %Duration{minute: -30}) == :eq
    #assert Comp.compare(post.two_times_duration_b, %Duration{hour: 1}) == :eq
    #assert Comp.compare(post.duration_b_times_three, %Duration{minute: 90}) == :eq
    assert Comp.compare(post.duration_a_negated , %Duration{hour: -1}) == :eq
  end
end
