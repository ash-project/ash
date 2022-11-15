defmodule Ash.SatSolver.Utils do
  @moduledoc false
  def replace_ordered_sublist([], _sublist, _replacement), do: []

  def replace_ordered_sublist([h | tail] = list, sublist, replacement) do
    if List.starts_with?(list, sublist) do
      [replacement | Enum.drop(list, length(sublist))]
    else
      [h | replace_ordered_sublist(tail, sublist, replacement)]
    end
  end

  def is_ordered_sublist_of?(_, []), do: false

  def is_ordered_sublist_of?(sublist, [_ | rest] = list) do
    List.starts_with?(list, sublist) || is_ordered_sublist_of?(sublist, rest)
  end

  def ordered_sublists(list) do
    front = sublists_front(list)

    back =
      list
      |> sublists_back()
      |> Enum.reject(&(&1 in front))

    front ++ back
  end

  def sublists_back([_, _]), do: []

  def sublists_back([]), do: []
  def sublists_back([_]), do: []

  def sublists_back(list) do
    list = :lists.droplast(list)
    [list | sublists_back(list)]
  end

  def sublists_front(list) do
    list
    |> do_sublists_front()
    |> Enum.reject(fn
      ^list ->
        true

      [_] ->
        true

      _ ->
        false
    end)
  end

  def do_sublists_front([]) do
    []
  end

  def do_sublists_front([_first | rest] = list) do
    [list | do_sublists_front(rest)]
  end
end
