defmodule YamlElixir do
  alias YamlElixir.Mapper

  @yamerl_options [
    detailed_constr: true,
    str_node_as_binary: true,
    keep_duplicate_keys: true
  ]

  def read_all_from_file!(path, options \\ []) do
    case read_all_from_file(path, options) do
      {:ok, result} -> result
      {:error, error} -> raise error
    end
  end

  def read_all_from_file(path, options \\ []), do: do_read(:file, path, options)

  def read_from_file!(path, options \\ []) do
    case read_from_file(path, options) do
      {:ok, result} -> result
      {:error, error} -> raise error
    end
  end

  def read_from_file(path, options \\ []),
    do: do_read(:file, path, Keyword.put(options, :one_result, true))

  def read_all_from_string!(string, options \\ []) do
    case read_all_from_string(string, options) do
      {:ok, result} -> result
      {:error, error} -> raise error
    end
  end

  def read_all_from_string(string, options \\ []), do: do_read(:string, string, options)

  def read_from_string!(string, options \\ []) do
    case read_from_string(string, options) do
      {:ok, result} -> result
      {:error, error} -> raise error
    end
  end

  def read_from_string(string, options \\ []),
    do: do_read(:string, string, Keyword.put(options, :one_result, true))

  defp do_read(type, source, options) do
    {:ok, prepare_and_read(type, source, options)}
  catch
    {:yamerl_exception, [{_, _, message, _, _, :file_open_failure, _, _}]} ->
      {:error, %YamlElixir.FileNotFoundError{message: List.to_string(message)}}

    {:yamerl_exception, [error | _]} ->
      {:error, YamlElixir.ParsingError.from_yamerl(error)}

    _, _ ->
      {:error, %YamlElixir.ParsingError{message: "malformed yaml"}}
  end

  defp prepare_and_read(type, source, options) do
    ensure_yamerl_started()

    options
    |> merge_options()
    |> read(type, source)
  end

  defp read(options, type, source) do
    type
    |> yamerl_constr(source, options)
    |> extract_data(options)
    |> Mapper.process(options)
  end

  defp merge_options(options), do: Keyword.merge(options, @yamerl_options)

  defp yamerl_constr(:file, path, options), do: :yamerl_constr.file(path, options)
  defp yamerl_constr(:string, data, options), do: :yamerl_constr.string(data, options)

  defp extract_data(data, options) do
    options
    |> Keyword.get(:one_result)
    |> maybe_take_last(data)
  end

  defp maybe_take_last(true, data), do: List.last(data)
  defp maybe_take_last(_, data), do: data

  defp ensure_yamerl_started, do: Application.start(:yamerl)
end
