defimpl Collectable, for: Iter do
  @doc false
  def into(iter), do: {iter, &collector/2}

  defp collector(iter, {:cont, elem}), do: Iter.append(iter, elem)
  defp collector(iter, :done), do: iter
  defp collector(_iter, :halt), do: :ok
end
