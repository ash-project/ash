defimpl Iter.IntoIterable, for: IO.Stream do
  @moduledoc false

  @doc false
  @impl true
  def into_iterable(%{device: device, raw: raw, line_or_bytes: line_or_bytes}) do
    next_fun =
      case raw do
        true -> &IO.each_binstream(&1, line_or_bytes)
        false -> &IO.each_stream(&1, line_or_bytes)
      end

    Iter.Iterable.Resource.new(
      fn -> device end,
      next_fun,
      & &1
    )
  end
end
