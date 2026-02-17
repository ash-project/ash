defmodule Ymlr do
  @moduledoc """
  Encodes data into YAML documents using the `Ymlr.Encoder` protocol.
  Every document starts with a separator ("---") and can be enhanced with comments.
  """

  alias Ymlr.Encode
  alias Ymlr.Encoder

  @type document :: term() | {binary(), term()} | {[binary()], term()}

  @doc ~S"""
  Encodes a given data as YAML document with a separator ("---") at the beginning. Raises if it cannot be encoded.

  Optinally you can pass a tuple with comment(s) and data as first argument.

  ## Options

  * `atoms` - when set to `true`, encodes atom map keys with a leading colon.
  * `sort_maps` - when set to `true`, sorts map by key-values.

  ## Examples

      iex> Ymlr.document!(%{a: 1})
      "---\na: 1\n"

      iex> Ymlr.document!(%{a: 1}, atoms: true)
      "---\n:a: 1\n"

      iex> Ymlr.document!({"comment", %{a: 1}})
      "---\n# comment\na: 1\n"

      iex> Ymlr.document!({["comment 1", "comment 2"], %{a: 1}})
      "---\n# comment 1\n# comment 2\na: 1\n"

      iex> Ymlr.document!(Map.new(1..33, &{&1, &1}), sort_maps: true)
      "---\n1: 1\n2: 2\n3: 3\n4: 4\n5: 5\n6: 6\n7: 7\n8: 8\n9: 9\n10: 10\n11: 11\n12: 12\n13: 13\n14: 14\n15: 15\n16: 16\n17: 17\n18: 18\n19: 19\n20: 20\n21: 21\n22: 22\n23: 23\n24: 24\n25: 25\n26: 26\n27: 27\n28: 28\n29: 29\n30: 30\n31: 31\n32: 32\n33: 33\n"
  """
  @spec document!(document, opts :: Keyword.t()) :: binary()
  def document!(document, opts \\ [])

  def document!({lines, data}, opts) when is_list(lines) do
    comments = Enum.map_join(lines, "", &"# #{&1}\n")
    "---\n" <> comments <> Encode.to_s!(data, opts) <> "\n"
  end

  def document!({comment, data}, opts), do: document!({[comment], data}, opts)

  def document!(data, opts) do
    document!({[], data}, opts)
  end

  @doc ~S"""
  Encodes a given data as YAML document with a separator ("---") at the beginning.

  Optinally you can pass a tuple with comment(s) and data as first argument.

  ## Options

  * `atoms` - when set to `true`, encodes atom map keys with a leading colon.
  * `sort_maps` - when set to `true`, sorts map by key-values.

  ## Examples

      iex> Ymlr.document(%{a: 1})
      {:ok, "---\na: 1\n"}

      iex> Ymlr.document(%{a: 1}, atoms: true)
      {:ok, "---\n:a: 1\n"}

      iex> Ymlr.document({"comment", %{a: 1}})
      {:ok, "---\n# comment\na: 1\n"}

      iex> Ymlr.document({["comment 1", "comment 2"], %{a: 1}})
      {:ok, "---\n# comment 1\n# comment 2\na: 1\n"}

      iex> Ymlr.document(Map.new(1..33, &{&1, &1}), sort_maps: true)
      {:ok, "---\n1: 1\n2: 2\n3: 3\n4: 4\n5: 5\n6: 6\n7: 7\n8: 8\n9: 9\n10: 10\n11: 11\n12: 12\n13: 13\n14: 14\n15: 15\n16: 16\n17: 17\n18: 18\n19: 19\n20: 20\n21: 21\n22: 22\n23: 23\n24: 24\n25: 25\n26: 26\n27: 27\n28: 28\n29: 29\n30: 30\n31: 31\n32: 32\n33: 33\n"}
  """
  @spec document(document, opts :: Encoder.opts()) :: {:ok, binary()} | {:error, binary()}
  def document(document, opts \\ []) do
    yml = document!(document, opts)
    {:ok, yml}
  rescue
    e in Protocol.UndefinedError -> {:error, Exception.message(e)}
  end

  @doc ~S"""
  Encodes a given list of data as "---" separated YAML documents. Raises if it cannot be encoded.

  ## Options

  * `atoms` - when set to `true`, encodes atom map keys with a leading colon.
  * `sort_maps` - when set to `true`, sorts map by key-values.

  ## Examples

      iex> Ymlr.documents!([%{a: 1}])
      "---\na: 1\n"

      iex> Ymlr.documents!([%{a: 1}], atoms: true)
      "---\n:a: 1\n"

      iex> Ymlr.documents!([%{a: 1}, %{b: 2}])
      "---\na: 1\n\n---\nb: 2\n"

      iex> Ymlr.documents!(%{a: "a"})
      ** (ArgumentError) The given argument is not a list of documents. Use document/1, document/2, document!/1 or document!/2 for a single document.
  """
  def documents!(documents, opts \\ [])

  def documents!(documents, opts) when is_list(documents),
    do: Enum.map_join(documents, "\n", &document!(&1, opts))

  def documents!(_documents, _opts),
    do:
      raise(
        ArgumentError,
        "The given argument is not a list of documents. Use document/1, document/2, document!/1 or document!/2 for a single document."
      )

  @doc ~S"""
  Encodes a given list of data as "---" separated YAML documents.

  ## Options

  * `atoms` - when set to `true`, encodes atom map keys with a leading colon.
  * `sort_maps` - when set to `true`, sorts map by key-values.

  ## Examples

      iex> Ymlr.documents([%{a: 1}])
      {:ok, "---\na: 1\n"}

      iex> Ymlr.documents([%{a: 1}], atoms: true)
      {:ok, "---\n:a: 1\n"}

      iex> Ymlr.documents([%{a: 1}, %{b: 2}])
      {:ok, "---\na: 1\n\n---\nb: 2\n"}

      iex> Ymlr.documents(%{a: "a"})
      {:error, "The given argument is not a list of documents. Use document/1, document/2, document!/1 or document!/2 for a single document."}
  """
  @spec documents([document], opts :: Encoder.opts()) :: {:ok, binary()} | {:error, binary()}
  def documents(documents, opts \\ []) do
    yml = documents!(documents, opts)
    {:ok, yml}
  rescue
    e in Protocol.UndefinedError ->
      {:error, Exception.message(e)}

    e in ArgumentError ->
      {:error, Exception.message(e)}
  end
end
