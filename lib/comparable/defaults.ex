import Comp

defcomparable left :: Any, right :: Any do
  case {left, right} do
    {_, _} when left == right ->
      Comp.eq()

    {%name{}, %name{}} ->
      left
      |> Map.from_struct()
      |> Comp.compare(Map.from_struct(right))

    {_, _} when left > right ->
      Comp.gt()

    {_, _} when left < right ->
      Comp.lt()
  end
end

defcomparable left :: List, right :: List do
  left
  |> Stream.zip(right)
  |> Enum.reduce_while(Comp.eq(), fn {lx, rx}, Comp.eq() ->
    lx
    |> Comp.compare(rx)
    |> case do
      res when res in [Comp.gt(), Comp.lt()] -> {:halt, res}
      Comp.eq() = res -> {:cont, res}
    end
  end)
  |> case do
    res when res in [Comp.gt(), Comp.lt()] ->
      res

    Comp.eq() ->
      left_length = length(left)
      right_length = length(right)

      cond do
        left_length > right_length -> Comp.gt()
        left_length < right_length -> Comp.lt()
        true -> Comp.eq()
      end
  end
end

defcomparable left :: Map, right :: Map do
  left_length = map_size(left)
  right_length = map_size(right)

  cond do
    left_length > right_length ->
      Comp.gt()

    left_length < right_length ->
      Comp.lt()

    true ->
      left
      |> Map.keys()
      |> Comp.compare(right |> Map.keys())
      |> case do
        res when res in [Comp.gt(), Comp.lt()] ->
          res

        Comp.eq() ->
          left
          |> Map.values()
          |> Comp.compare(right |> Map.values())
      end
  end
end

defcomparable left :: Tuple, right :: Tuple do
  left_length = tuple_size(left)
  right_length = tuple_size(right)

  cond do
    left_length > right_length ->
      Comp.gt()

    left_length < right_length ->
      Comp.lt()

    true ->
      left
      |> Tuple.to_list()
      |> Comp.compare(right |> Tuple.to_list())
  end
end
