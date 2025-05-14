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

      attribute :duration_c, :duration, allow_nil?: true, public?: true

      attribute :duration_d, :duration, allow_nil?: true, public?: true

      attribute :date, :date, allow_nil?: true, public?: true

      attribute :datetime, :datetime, allow_nil?: true, public?: true

      attribute :naive_datetime, :naive_datetime, allow_nil?: true, public?: true

      attribute :time, :time, allow_nil?: true, public?: true

      attribute :time_usec, :time_usec, allow_nil?: true, public?: true

      attribute :utc_datetime, :utc_datetime, allow_nil?: true, public?: true

      attribute :utc_datetime_usec, :utc_datetime_usec, allow_nil?: true, public?: true
    end

    calculations do
      calculate :duration_a_plus_b, :duration, expr(duration_a + duration_b)
      calculate :duration_b_minus_a, :duration, expr(duration_b - duration_a)
      calculate :duration_b_times_three, :duration, expr(duration_b * 3)
      calculate :duration_a_negated, :duration, expr(-duration_a)
      calculate :date_plus_duration_d, :date, expr(datetime + duration_d)
      calculate :date_minus_duration_d, :date, expr(datetime - duration_d)
      calculate :datetime_plus_duration_a, :datetime, expr(datetime + duration_a)
      calculate :datetime_minus_duration_b, :datetime, expr(datetime - duration_b)

      calculate :naive_datetime_plus_duration_a,
                :naive_datetime,
                expr(naive_datetime + duration_a)

      calculate :naive_datetime_minus_duration_b,
                :naive_datetime,
                expr(naive_datetime - duration_b)

      calculate :time_plus_duration_a, :time, expr(time + duration_a)
      calculate :time_minus_duration_b, :time, expr(time - duration_b)
      calculate :time_usec_plus_duration_c, :time_usec, expr(time_usec + duration_c)
      calculate :time_usec_minus_duration_c, :time_usec, expr(time_usec - duration_c)
      calculate :utc_datetime_plus_duration_a, :utc_datetime, expr(utc_datetime + duration_a)
      calculate :utc_datetime_minus_duration_b, :utc_datetime, expr(utc_datetime - duration_b)

      calculate :utc_datetime_usec_plus_duration_c,
                :utc_datetime_usec,
                expr(utc_datetime_usec + duration_c)

      calculate :utc_datetime_usec_minus_duration_c,
                :utc_datetime_usec,
                expr(utc_datetime_usec - duration_c)
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
        duration_a: @hour1,
        duration_b: @minute30
      })
      |> Ash.create!()

    assert post.duration_a == %Duration{hour: 1}
    assert post.duration_b == %Duration{minute: 30}
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
