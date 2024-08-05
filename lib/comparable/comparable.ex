unless Code.ensure_loaded?(Comparable) do
  defprotocol Comparable do
    @moduledoc """
    Protocol which describes ordering relation for pair of types
    """

    @type t :: Comparable.t()
    @type ord :: :gt | :lt | :eq

    @doc """
    Accepts struct with fields :left and :right and returns ord value
    """
    @spec compare(t) :: ord
    def compare(left_and_right)
  end
end
