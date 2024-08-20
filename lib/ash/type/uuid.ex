defmodule Ash.Type.UUID do
  @moduledoc """
  Represents a UUID.

  A builtin type that can be referenced via `:uuid`
  """

  use Ash.Type

  @impl true
  def storage_type(_), do: :uuid

  @impl true
  def generator(_constraints) do
    StreamData.repeatedly(&Ash.UUID.generate/0)
  end

  @impl true
  def matches_type?(v, constraints) do
    case cast_input(v, constraints) do
      {:ok, _} -> true
      _ -> false
    end
  end

  @impl true
  def cast_input(nil, _), do: {:ok, nil}

  def cast_input(%Ash.CiString{string: string}, constraints), do: cast_input(string, constraints)

  def cast_input(
        <<a1, a2, a3, a4, a5, a6, a7, a8, ?-, b1, b2, b3, b4, ?-, c1, c2, c3, c4, ?-, d1, d2, d3,
          d4, ?-, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12>>,
        _
      ) do
    <<c(a1), c(a2), c(a3), c(a4), c(a5), c(a6), c(a7), c(a8), ?-, c(b1), c(b2), c(b3), c(b4), ?-,
      c(c1), c(c2), c(c3), c(c4), ?-, c(d1), c(d2), c(d3), c(d4), ?-, c(e1), c(e2), c(e3), c(e4),
      c(e5), c(e6), c(e7), c(e8), c(e9), c(e10), c(e11), c(e12)>>
  catch
    :error -> :error
  else
    hex_uuid -> {:ok, hex_uuid}
  end

  def cast_input(<<_::128>> = raw_uuid, _),
    do: {:ok, encode(raw_uuid)}

  def cast_input(value, _) when is_binary(value) do
    case String.trim(value) do
      <<a1, a2, a3, a4, a5, a6, a7, a8, ?-, b1, b2, b3, b4, ?-, c1, c2, c3, c4, ?-, d1, d2, d3,
        d4, ?-, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12>> ->
        try do
          <<c(a1), c(a2), c(a3), c(a4), c(a5), c(a6), c(a7), c(a8), ?-, c(b1), c(b2), c(b3),
            c(b4), ?-, c(c1), c(c2), c(c3), c(c4), ?-, c(d1), c(d2), c(d3), c(d4), ?-, c(e1),
            c(e2), c(e3), c(e4), c(e5), c(e6), c(e7), c(e8), c(e9), c(e10), c(e11), c(e12)>>
        catch
          :error -> :error
        else
          hex_uuid -> {:ok, hex_uuid}
        end

      _ ->
        :error
    end
  end

  def cast_input(_, _), do: :error

  @impl true
  def cast_atomic(new_value, _constraints) do
    {:atomic, new_value}
  end

  @impl true
  def cast_stored(nil, _), do: {:ok, nil}

  def cast_stored(value, constraints) do
    case Ecto.Type.load(Ecto.UUID, value) do
      :error ->
        cast_input(value, constraints)

      {:ok, value} ->
        {:ok, value}
    end
  rescue
    _e in ArgumentError ->
      cast_input(value, constraints)
  end

  @impl true
  def dump_to_embedded(value, constraints) do
    cast_input(value, constraints)
  end

  @impl true

  def dump_to_native(nil, _), do: {:ok, nil}

  def dump_to_native(value, _) do
    Ecto.Type.dump(Ecto.UUID, value)
  end

  @compile {:inline, c: 1}

  defp c(?0), do: ?0
  defp c(?1), do: ?1
  defp c(?2), do: ?2
  defp c(?3), do: ?3
  defp c(?4), do: ?4
  defp c(?5), do: ?5
  defp c(?6), do: ?6
  defp c(?7), do: ?7
  defp c(?8), do: ?8
  defp c(?9), do: ?9
  defp c(?A), do: ?a
  defp c(?B), do: ?b
  defp c(?C), do: ?c
  defp c(?D), do: ?d
  defp c(?E), do: ?e
  defp c(?F), do: ?f
  defp c(?a), do: ?a
  defp c(?b), do: ?b
  defp c(?c), do: ?c
  defp c(?d), do: ?d
  defp c(?e), do: ?e
  defp c(?f), do: ?f
  defp c(_), do: throw(:error)

  defp encode(
         <<a1::4, a2::4, a3::4, a4::4, a5::4, a6::4, a7::4, a8::4, b1::4, b2::4, b3::4, b4::4,
           c1::4, c2::4, c3::4, c4::4, d1::4, d2::4, d3::4, d4::4, e1::4, e2::4, e3::4, e4::4,
           e5::4, e6::4, e7::4, e8::4, e9::4, e10::4, e11::4, e12::4>>
       ) do
    <<e(a1), e(a2), e(a3), e(a4), e(a5), e(a6), e(a7), e(a8), ?-, e(b1), e(b2), e(b3), e(b4), ?-,
      e(c1), e(c2), e(c3), e(c4), ?-, e(d1), e(d2), e(d3), e(d4), ?-, e(e1), e(e2), e(e3), e(e4),
      e(e5), e(e6), e(e7), e(e8), e(e9), e(e10), e(e11), e(e12)>>
  end

  @compile {:inline, e: 1}

  defp e(0), do: ?0
  defp e(1), do: ?1
  defp e(2), do: ?2
  defp e(3), do: ?3
  defp e(4), do: ?4
  defp e(5), do: ?5
  defp e(6), do: ?6
  defp e(7), do: ?7
  defp e(8), do: ?8
  defp e(9), do: ?9
  defp e(10), do: ?a
  defp e(11), do: ?b
  defp e(12), do: ?c
  defp e(13), do: ?d
  defp e(14), do: ?e
  defp e(15), do: ?f
end
