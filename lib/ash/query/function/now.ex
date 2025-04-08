defmodule Ash.Query.Function.Now do
  @moduledoc """
  Returns the current datetime
  """
  use Ash.Query.Function, name: :now, eager_evaluate?: false

  def args, do: [[], [:time_zone], [:time_zone, :time_zone_database]]

  def returns, do: [:utc_datetime_usec, :datetime]

  def evaluate(%{arguments: [timezone]}), do: {:known, DateTime.now!(timezone)}

  def evaluate(%{arguments: [time_zone, time_zone_database]}),
    do: {:known, DateTime.now!(time_zone, time_zone_database)}

  def evaluate(_), do: {:known, DateTime.utc_now()}

  def can_return_nil?(_), do: false
end
