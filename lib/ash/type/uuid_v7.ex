defmodule Ash.Type.UUIDv7 do
  @moduledoc """
  Represents a UUID.

  A builtin type that can be referenced via `:uuid_v7`
  """

  use Ash.Type

  @impl true
  def storage_type(_), do: :uuid

  @impl true
  def generator(_constraints) do
    StreamData.repeatedly(&Ash.UUIDv7.generate/0)
  end

  @impl true
  def matches_type?(value, constraints) do
    case cast_input(value, constraints) do
      {:ok, _} -> true
      _ -> false
    end
  end

  @impl true
  def cast_input(%Ash.CiString{string: string}, constraints), do: cast_input(string, constraints)
  def cast_input(nil, _), do: {:ok, nil}

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

  def cast_input(<<_::128>> = value, _), do: {:ok, Ash.UUIDv7.encode(value)}
  def cast_input(_, _), do: :error

  @impl true
  def cast_atomic(new_value, _constraints), do: {:atomic, new_value}

  @impl true
  def cast_stored(nil, _), do: {:ok, nil}
  def cast_stored(value, constraints), do: cast_input(value, constraints)

  @impl true
  def dump_to_native(nil, _), do: {:ok, nil}

  def dump_to_native(value, _) do
    case Ash.UUIDv7.decode(value) do
      :error -> :error
      value -> {:ok, value}
    end
  end

  @impl true
  def dump_to_embedded(value, constraints) do
    cast_input(value, constraints)
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
end
