defimpl Iter.Iterable, for: Range do
  @moduledoc false

  use Iter.Impl

  @doc false
  @impl true
  def next(%{first: first, last: last, step: step}) when first > last and step > 0, do: :done
  def next(%{first: first, last: last, step: step}) when first < last and step < 0, do: :done

  def next(%{first: first, step: step} = range) do
    {:ok, first, %{range | first: first + step}}
  end

  @doc false
  @impl true
  def step_by(range, step_size), do: %{range | step: step_size}

  @doc false
  @impl true
  def member?(range, element) when is_integer(element),
    do: do_member?(range, element)

  def member?(_, _), do: false

  defp do_member?(range, element) when range.first == element,
    do: true

  defp do_member?(range, element)
       when range.step == 1 and element >= range.first and element <= range.last,
       do: true

  defp do_member?(range, element) when element > range.last,
    do: false

  defp do_member?(range, element),
    do: rem(element - range.first, range.step) == 0

  @doc false
  @impl true
  def dedup(range), do: range

  @doc false
  @impl true
  def uniq(range), do: range
end
