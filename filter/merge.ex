defmodule Ash.Filter.Merge do
  @moduledoc false
  alias Ash.Filter.And

  def merge(left, right) do
    [left, right] = Enum.sort_by([left, right], fn %mod{} -> to_string(mod) end)

    do_merge(left, right)
  end

  defp do_merge(left, right) do
    And.prebuilt_new(left, right)
  end
end
