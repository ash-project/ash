defimpl Iter.IntoIterable, for: File.Stream do
  @moduledoc false

  @doc false
  @impl true
  def into_iterable(%{modes: modes, line_or_bytes: line_or_bytes, raw: raw} = stream) do
    start_fun = fn ->
      case File.Stream.__open__(stream, read_modes(modes)) do
        {:ok, device} ->
          skip_bom_and_offset(device, raw, modes)

        {:error, reason} ->
          raise File.Error, reason: reason, action: "stream", path: stream.path
      end
    end

    next_fun =
      case raw do
        true -> &IO.each_binstream(&1, line_or_bytes)
        false -> &IO.each_stream(&1, line_or_bytes)
      end

    Iter.Iterable.Resource.new(start_fun, next_fun, &:file.close/1)
  end

  defp read_modes(modes) do
    for mode <- modes, mode not in [:write, :append, :trim_bom], do: mode
  end

  defp skip_bom_and_offset(device, raw, modes) do
    device =
      if :trim_bom in modes do
        device |> trim_bom(raw) |> elem(0)
      else
        device
      end

    offset = get_read_offset(modes)

    if offset > 0 do
      {:ok, _} = :file.position(device, {:cur, offset})
    end

    device
  end

  defp trim_bom(device, true) do
    bom_length = device |> IO.binread(4) |> bom_length()
    {:ok, new_pos} = :file.position(device, bom_length)
    {device, new_pos}
  end

  defp trim_bom(device, false) do
    # Or we read the bom in the correct amount or it isn't there
    case bom_length(IO.read(device, 1)) do
      0 ->
        {:ok, _} = :file.position(device, 0)
        {device, 0}

      _ ->
        {device, 1}
    end
  end

  defp bom_length(<<239, 187, 191, _rest::binary>>), do: 3
  defp bom_length(<<254, 255, _rest::binary>>), do: 2
  defp bom_length(<<255, 254, _rest::binary>>), do: 2
  defp bom_length(<<0, 0, 254, 255, _rest::binary>>), do: 4
  defp bom_length(<<254, 255, 0, 0, _rest::binary>>), do: 4
  defp bom_length(_binary), do: 0

  def get_read_offset(modes) do
    case :lists.keyfind(:read_offset, 1, modes) do
      {:read_offset, offset} -> offset
      false -> 0
    end
  end
end
