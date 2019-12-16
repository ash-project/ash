defmodule Ash.Filter.Merge do
  alias Ash.Filter.{In, Eq, And, Impossible}

  def merge(left, right) do
    [left, right] = Enum.sort_by([left, right], fn %mod{} -> to_string(mod) end)

    do_merge(left, right)
  end

  ## Because we don't support any kind of "or" filter, right now, all combinations
  ## are "and" which means an impossible + anything = impossible
  # defp do_merge(%Impossible{} = left, _), do: left
  # defp do_merge(_, %Impossible{} = right), do: right

  ## Equals + Equals
  # defp do_merge(%Eq{value: left_value} = left, %Eq{values: right_value}) do
  #   if left_value == right_value do
  #     left
  #   else
  #     %Impossible{cause: :duplicate_equal}
  #   end
  # end

  ## Equals + In filters

  # defp do_merge(%Eq{value: value} = left, %In{values: values}) do
  #   if value in values do
  #     left
  #   else
  #     %Impossible{cause: :equal_in_list}
  #   end
  # end

  ## In + In filters

  # defp do_merge(%In{values: left}, %In{values: right}) do
  #   values =
  #     left
  #     |> MapSet.new()
  #     |> MapSet.intersection(MapSet.new(right))
  #     |> MapSet.to_list()

  #   %In{values: values}
  # end

  defp do_merge(left, right) do
    # There is no way this can reasonably fail
    {:ok, predicate} = And.new(left, right)

    predicate
  end

  ## Equals + In filters
end
