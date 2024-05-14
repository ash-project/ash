defmodule Ash.Query.Function.Now do
  @moduledoc """
  Returns the current datetime
  """
  use Ash.Query.Function, name: :now, eager_evaluate?: false

  def args, do: [[]]

  def evaluate(_), do: {:known, DateTime.utc_now()}

  def can_return_nil?(_), do: false
end
