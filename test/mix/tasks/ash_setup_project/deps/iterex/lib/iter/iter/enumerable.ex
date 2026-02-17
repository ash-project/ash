defimpl Enumerable, for: Iter do
  alias Iter.Iterable

  @moduledoc false

  @doc false
  def count(iter), do: {:ok, Iterable.count(iter.iterable)}

  @doc false
  def member?(iter, element), do: {:ok, Iterable.member?(iter.iterable, element)}

  @doc false
  def reduce(_iter, {:halt, acc}, _fun), do: {:halted, acc}
  def reduce(iter, {:suspend, acc}, fun), do: {:suspended, acc, &reduce(iter, &1, fun)}

  def reduce(iter, {:cont, acc}, fun) do
    case Iterable.next(iter.iterable) do
      {:ok, element, iterable} -> reduce(Iter.from(iterable), fun.(element, acc), fun)
      :done -> {:done, acc}
    end
  end

  @doc false
  def slice(_iter), do: {:error, __MODULE__}
end
