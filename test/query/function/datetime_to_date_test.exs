defmodule Ash.Query.Function.DateTimeToDateTest do
  use ExUnit.Case, async: true

  alias Ash.Query.Function.DateTimeToDate

  defmodule Event do
    use Ash.Resource,
      domain: Ash.Test.Domain,
      data_layer: Ash.DataLayer.Ets

    attributes do
      integer_primary_key :id

      attribute :name, :string do
        public? true
        allow_nil? true
      end

      attribute :begin_at, :datetime do
        public? true
        allow_nil? true
      end

      attribute :end_at, :naive_datetime do
        public? true
        allow_nil? true
      end
    end

    actions do
      default_accept :*

      defaults [:create, :read]
    end

    calculations do
      calculate :begin_at_date, :date, expr(datetime_to_date(begin_at))
      calculate :end_at_date, :date, expr(datetime_to_date(end_at))
    end
  end

  describe "when called directly" do
    test "returns the Date of the given DateTime" do
      datetime = ~U[2024-10-15 14:40:00.602331Z]
      date = ~D[2024-10-15]

      assert {:known, ^date} = DateTimeToDate.evaluate(%{arguments: [datetime]})
    end

    test "returns the Date of the given NaiveDateTime" do
      datetime = ~N[2024-10-15 14:40:00.602331]
      date = ~D[2024-10-15]

      assert {:known, ^date} = DateTimeToDate.evaluate(%{arguments: [datetime]})
    end
  end

  describe "when called via a calculation" do
    test "returns the Date of the given DateTime" do
      event =
        Ash.create!(Event, %{
          begin_at: ~U[2024-10-15 14:00:00.000000Z],
          end_at: ~N[2024-11-15 18:00:00.000000]
        })
        |> Ash.load!([:begin_at_date, :end_at_date])

      assert event.begin_at_date == ~D[2024-10-15]
      assert event.end_at_date == ~D[2024-11-15]
    end
  end
end
