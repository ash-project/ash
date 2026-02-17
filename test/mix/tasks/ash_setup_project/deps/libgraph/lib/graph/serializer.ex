defmodule Graph.Serializer do
  @moduledoc """
  This module defines the Serializer behavior for graphs.
  """
  @callback serialize(Graph.t()) :: {:ok, binary} | {:error, term}

  defmacro __using__(_) do
    quote do
      @behaviour Graph.Serializer
    end
  end

  def get_vertex_label(%Graph{vertex_labels: vl}, id, v) do
    case Map.get(vl, id) do
      [] -> encode_label(v)
      label -> encode_label(label)
    end
  end

  def encode_label([h | _] = label) when length(label) == 1, do: encode_label(h)
  def encode_label(label) when is_binary(label), do: quoted(label)
  def encode_label(label) when is_integer(label), do: Integer.to_string(label)
  def encode_label(label) when is_float(label), do: Float.to_string(label)
  def encode_label(label) when is_atom(label), do: quoted(Atom.to_string(label))
  def encode_label(label), do: quoted("#{inspect(label)}")

  def quoted(str) do
    <<?", escape_quotes(str)::binary, ?">>
  end

  def escape_quotes(str) do
    escape_quotes(str, "")
  end

  def escape_quotes(<<>>, acc), do: acc

  def escape_quotes(<<?\\, ?\", rest::binary>>, acc) do
    escape_quotes(rest, <<acc::binary, ?\\, ?\">>)
  end

  def escape_quotes(<<?\", rest::binary>>, acc) do
    escape_quotes(rest, <<acc::binary, ?\\, ?\">>)
  end

  def escape_quotes(<<c::utf8, rest::binary>>, acc) do
    escape_quotes(rest, <<acc::binary, c::utf8>>)
  end

  def indent(tabs), do: String.duplicate(" ", tabs * 4)
end
