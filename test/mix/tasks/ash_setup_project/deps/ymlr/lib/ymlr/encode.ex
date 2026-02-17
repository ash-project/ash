defmodule Ymlr.Encode do
  @moduledoc ~S"""

  This module implements the logic of encoding scalars.

  ## Strings and Characters

  ### Printable Characters

  The YAML spec defines a set of printable characters `c-printable` (see
  https://yaml.org/spec/1.2.2/#character-set). All these characters can
  theoretically be left alone when encoding a string.

  ### Escape Characters

  The YAML spec also defines a set of escape charactesr `c-ns-esc-char` (see
  https://yaml.org/spec/1.2.2/#57-escaped-characters). Some of these chars are
  also in the printable range `c-printable`. Being in `c-printable` means they
  could be left alone. I.e. there would be no need to encode them as escape
  chars. However, we think in certain cases, escape characters are more
  reader friendly than the actual characters. An example is the "next line"
  character (`U+0085` or `\N`). It is part of `c-printable`. However, on the
  screen this character cannot be distinguished from a simple "line feed"
  character (`U+000A` or `\n`). Therefore all characters in `c-ns-esc-char` with
  the exception of `\n` and `\t` are always encoded using their escape character.

  ### Other 8-bit Unicode Characters

  Any 8-bit unicode character that neither a printable nor an escape character
  has to be encoded using one of the three unicode escape characters \x, \u or
  \U (i.e. \xXX, \u00XX or \U000000XX).

  ### Double Quotes for Escape Characters

  Printable Characters can be encoded unquoted, single-quoted or double-quoted.
  Escape characters require double quotes.

  ### Chars with Special Treatments

  #### Chars `\n` and `\t`

  These two characters are never converted to their escape characters.
  One exception: If the given string is literally just a newline, we
  encode it as "\n" (double quotes required for escape chars) rather than a
  single newline.

  #### Chars `"` and `\`

  These two characters have escape characters (`\"` and `\\`) but they are also
  part of the of the printable character range `c-printable` and they have a
  well-defined presentation on the screen. Ocurrance of these characters don't
  enforce double-quotes but if they occur within a string that for other reasons
  requires double-quotes, they need to be escaped.

  ### Implemented Decision Logic

  First matching rule is applied:

  1. Char is `\t` or `\n` => leave alone
  1. Char is `"` or `\` => if within double quotes, escape. Otherwise leave alone.
  1. Char has an escape character (i.e. is part of `c-ns-esc-char`) => force double quotes and encode as escape character
  1. Char is a printable character => leave alone
  1. Char is a non-printable character => force double quotes and encode as \xXX (only 8-bit supported for now)
  """

  alias Ymlr.Encoder

  @quote_when_starts_with_strings [
    " ",
    # tag
    "!",
    # anchor
    "&",
    # alias
    "*",
    # flow mapping
    "{",
    "}",
    # flow sequence
    "[",
    "]",
    # flow collection entry separator
    ",",
    # comment
    "#",
    # block scalar
    "|",
    ">",
    # reserved characters
    "@",
    "`",
    # double and single quotes
    "\"",
    "- ",
    ": ",
    ":{",
    "%",
    "? ",
    "0b",
    "0o",
    "0x",
    ".inf",
    ".Inf",
    ".INF",
    "+.inf",
    "+.Inf",
    "+.INF",
    "-.inf",
    "-.Inf",
    "-.INF",
    ".nan",
    ".Nan",
    ".NAN"
  ]

  @special_atom_mapping %{
    :nan => ".nan",
    :inf => ".inf",
    :"-inf" => "-.inf",
    :"+inf" => ".inf"
  }

  @quote_when_contains_string [" #", ": "]

  @quote_when_last_char [?\s, ?:]

  @single_quote_when_exact [
    "",
    "~",
    "?",
    "-",
    "null",
    "Null",
    "NULL",
    "y",
    "Y",
    "n",
    "N",
    "yes",
    "Yes",
    "YES",
    "no",
    "No",
    "NO",
    "true",
    "True",
    "TRUE",
    "false",
    "False",
    "FALSE",
    "on",
    "On",
    "ON",
    "off",
    "Off",
    "OFF"
  ]

  # Escape chars that, if contained within, force the string to be double-quoted:
  @escape_chars ~c"\a\b\e\f\r\v\0\u00a0\u0085\u2028\u2029\"\\"
  @escaped_chars ~c"abefrv0_NLP\"\\"
  @escape_if_within_double_quotes_mapping Enum.zip(@escape_chars, @escaped_chars)

  # coveralls-ignore-start - trivial code
  # Printable Characters:
  defguard is_printable(char)
           when char in 0x20..0x7E or char in ~c"\t\n\u0085" or char in 0xA0..0xFFFD or
                  char in 0x010000..0x10FFFF

  # Characters that need to be escaped as unicode \xHH or \uHHHH or \UHHHHHH:
  defguard requires_unicode_escape(char)
           when not is_printable(char) and char not in @escape_chars

  # Chars that, if contained within, force the string to be double-quoted:
  @escape_chars_forcing_double_quotes @escape_chars -- ~c"\"\\"
  defguard force_double_quote(char)
           when not is_printable(char) or char in @escape_chars_forcing_double_quotes

  # coveralls-ignore-stop

  @doc ~S"""
  Encodes the given data as YAML string. Raises if it cannot be encoded.

  ## Examples

      iex> Ymlr.Encode.to_s!(%{})
      "{}"

      iex> Ymlr.Encode.to_s!(%{a: 1, b: 2}, sort_maps: true)
      "a: 1\nb: 2"

      iex> Ymlr.Encode.to_s!(%{"a" => "a", "b" => :b, "c" => "true", "d" => "100"})
      "a: a\nb: b\nc: 'true'\nd: '100'"
  """
  @spec to_s!(data :: term(), opts :: Encoder.opts()) :: binary()
  def to_s!(data, opts \\ []) do
    data
    |> Ymlr.Encoder.encode(0, opts)
    |> IO.iodata_to_binary()
  end

  @doc ~S"""
  Encodes the given data as YAML string.

  ## Examples

      iex> Ymlr.Encode.to_s(%{a: 1, b: 2}, sort_maps: true)
      {:ok, "a: 1\nb: 2"}
  """
  @spec to_s(data :: term(), opts :: Encoder.opts()) :: {:ok, binary()} | {:error, binary()}
  def to_s(data, opts \\ []) do
    yml = to_s!(data, opts)
    {:ok, yml}
  rescue
    e in Protocol.UndefinedError -> {:error, Exception.message(e)}
  end

  @spec map(data :: map(), indent_level :: integer, opts :: Encoder.opts()) :: iodata()
  def map(data, _indent_level, _opts) when data == %{}, do: "{}"

  def map(data, indent_level, opts) when is_map(data) do
    indentation = indent(indent_level)
    key_encoder = if opts[:atoms], do: &encode_map_key_atoms/1, else: &encode_map_key/1
    data = if opts[:sort_maps], do: Enum.sort(data), else: data

    data
    |> Enum.map(fn
      {key, nil} ->
        key_encoder.(key)

      {key, value} when value == [] ->
        [key_encoder.(key), " []"]

      {key, value} when value == %{} ->
        [key_encoder.(key), " {}"]

      {key, value} when is_struct(value, Date) ->
        [key_encoder.(key), " " | Encoder.encode(value, indent_level, opts)]

      {key, value} when is_struct(value, Time) ->
        [key_encoder.(key), " " | Encoder.encode(value, indent_level, opts)]

      {key, value} when is_struct(value, NaiveDateTime) ->
        [key_encoder.(key), " " | Encoder.encode(value, indent_level, opts)]

      {key, value} when is_struct(value, DateTime) ->
        [key_encoder.(key), " " | Encoder.encode(value, indent_level, opts)]

      {key, value} when is_map(value) ->
        [key_encoder.(key), indentation, "  " | Encoder.encode(value, indent_level + 1, opts)]

      {key, value} when is_list(value) ->
        [key_encoder.(key), indentation, "  " | Encoder.encode(value, indent_level + 1, opts)]

      {key, value} ->
        [key_encoder.(key), " " | Encoder.encode(value, indent_level + 1, opts)]
    end)
    |> Enum.intersperse(indentation)
  end

  @spec list(data :: list(), indent_level :: integer, opts :: Encoder.opts()) :: iodata()
  def list([], _, _), do: "[]"

  def list(data, indent_level, opts) do
    indentation = indent(indent_level)

    data
    |> Enum.map(fn
      nil -> ?-
      "" -> ~s(- "")
      value -> ["- " | Encoder.encode(value, indent_level + 1, opts)]
    end)
    |> Enum.intersperse(indentation)
  end

  @spec atom(atom()) :: iodata()
  for {input, encoded} <- @special_atom_mapping do
    def atom(unquote(input)), do: unquote(encoded)
  end

  def atom(data), do: Atom.to_string(data)

  @spec string(binary(), integer) :: iodata()
  def string(data, indent_level), do: encode_binary(data, indent_level)

  @spec number(number()) :: iodata()
  def number(data), do: "#{data}"

  defp encode_map_key_atoms(data) when is_atom(data), do: [?:, Atom.to_string(data), ?:]
  defp encode_map_key_atoms(data), do: encode_map_key(data)

  defp encode_map_key(data) when is_atom(data), do: [Atom.to_string(data), ?:]
  defp encode_map_key(data) when is_binary(data), do: [encode_binary(data, nil), ?:]
  defp encode_map_key(data) when is_number(data), do: ["#{data}:"]

  defp encode_map_key(data),
    do:
      raise(ArgumentError,
        message: "The given data #{inspect(data)} cannot be converted to YAML (map key)."
      )

  for data <- @single_quote_when_exact do
    encoded = ~s('#{data}')
    defp encode_binary(unquote(data), _), do: unquote(encoded)
  end

  defp encode_binary("\n", _), do: ~S("\n")

  defp encode_binary(data, indent_level) do
    case string_encoding_type(data) do
      :multiline -> multiline(data, indent_level)
      :double_quoted -> with_double_quotes(data)
      :single_quoted -> with_single_quotes(data)
      _ -> data
    end
  end

  defp string_encoding_type(<<?', rest::binary>>) do
    do_string_encoding_type(rest, :double_quoted)
  end

  for data <- @quote_when_starts_with_strings do
    defp string_encoding_type(<<unquote(data), rest::binary>>) do
      do_string_encoding_type(rest, :single_quoted)
    end
  end

  defp string_encoding_type(data) do
    if numeric?(data) do
      :single_quoted
    else
      do_string_encoding_type(data, nil)
    end
  end

  defp do_string_encoding_type(<<?\n, _::binary>>, _), do: :multiline

  defp do_string_encoding_type(<<char::utf8, rest::binary>>, _)
       when force_double_quote(char) do
    do_string_encoding_type(rest, :double_quoted)
  end

  defp do_string_encoding_type(<<?', rest::binary>>, nil) do
    do_string_encoding_type(rest, :maybe_double_quoted)
  end

  defp do_string_encoding_type(<<?', rest::binary>>, _) do
    do_string_encoding_type(rest, :double_quoted)
  end

  for data <- @quote_when_contains_string do
    defp do_string_encoding_type(<<unquote(data)::utf8, rest::binary>>, :double_quoted) do
      do_string_encoding_type(rest, :double_quoted)
    end

    defp do_string_encoding_type(<<unquote(data)::utf8, rest::binary>>, :maybe_double_quoted) do
      do_string_encoding_type(rest, :double_quoted)
    end

    defp do_string_encoding_type(<<unquote(data)::utf8, rest::binary>>, _quotation) do
      do_string_encoding_type(rest, :single_quoted)
    end
  end

  defp do_string_encoding_type(<<char::utf8>>, quotation) when char in @quote_when_last_char do
    case quotation do
      :double_quoted -> :double_quoted
      :maybe_double_quoted -> :double_quoted
      _ -> :single_quoted
    end
  end

  defp do_string_encoding_type(<<_::utf8, rest::binary>>, quotation) do
    do_string_encoding_type(rest, quotation)
  end

  defp do_string_encoding_type(_, quotation), do: quotation

  defp with_double_quotes(data), do: [?", escape(data), ?"]
  defp with_single_quotes(data), do: [?', data, ?']

  defp escape(data) do
    for <<char::utf8 <- data>>, do: escape_char(char)
  end

  for {char, escape_char} <- @escape_if_within_double_quotes_mapping do
    escaped = <<?\\, escape_char>>
    defp escape_char(unquote(char)), do: unquote(escaped)
  end

  defp escape_char(char) when requires_unicode_escape(char) and char <= 0xFF,
    do: List.to_string(:io_lib.format("\\x~2.16.0B", [char]))

  defp escape_char(char) when requires_unicode_escape(char) and char in 0x0100..0xFFFF,
    do: List.to_string(:io_lib.format("\\u~4.16.0B", [char]))

  # coveralls-ignore-start - We don't use this function currently.
  defp escape_char(char) when requires_unicode_escape(char),
    do: List.to_string(:io_lib.format("\\U~6.16.0B", [char]))

  # coveralls-ignore-stop

  defp escape_char(char), do: <<char::utf8>>

  # for example for map keys
  defp multiline(data, nil), do: inspect(data)
  # see https://yaml-multiline.info/

  # This is pure aesthetics: If we are on level 0 (no indentation), we add one
  # level of indentation to make it look a bit nicer.
  defp multiline(data, 0), do: multiline(data, 1)

  defp multiline(data, level) do
    indentation = indent(level)

    block =
      data
      |> String.replace_suffix("\n", "")
      |> String.split("\n")
      |> Enum.map(fn
        "" ->
          "\n"

        line ->
          [indentation, line]
      end)

    [block_chomping_indicator(data) | [block]]
  end

  defp block_chomping_indicator(data) do
    cond do
      String.ends_with?(data, "\n\n") -> "|+"
      String.ends_with?(data, "\n") -> "|"
      :otherwise -> "|-"
    end
  end

  for level <- 1..10 do
    result = IO.iodata_to_binary(["\n" | List.duplicate("  ", level)])
    defp indent(unquote(level)), do: unquote(result)
  end

  defp indent(level), do: ["\n" | List.duplicate("  ", level)]

  defp numeric?(string) do
    case Float.parse(string) do
      {_, ""} -> true
      _ -> false
    end
  rescue
    #  Apparently not needed anymore since Elixir 1.14. Left in for bc but stop covering.
    # coveralls-ignore-start
    _ ->
      false
      # coveralls-ignore-stop
  end
end
