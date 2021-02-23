defmodule Ash.Helpers do
  @moduledoc false

  @spec try_compile(term) :: :ok
  def try_compile(module) when is_atom(module) do
    Code.ensure_loaded(module)
    :ok
  end

  def try_compile(_), do: :ok

  def implements_behaviour?(module, behaviour) do
    :attributes
    |> module.module_info()
    |> Enum.flat_map(fn
      {:behaviour, value} -> List.wrap(value)
      _ -> []
    end)
    |> Enum.any?(&(&1 == behaviour))
  rescue
    _ ->
      false
  end

  # A restricted version of `:erlang.binary_to_term/2` that forbids
  # *executable* terms, such as anonymous functions.
  # The `opts` are given to the underlying `:erlang.binary_to_term/2`
  # call, with an empty list as a default.
  # By default this function does not restrict atoms, as an atom
  # interned in one node may not yet have been interned on another
  # (except for releases, which preload all code).
  # If you want to avoid atoms from being created, then you can pass
  # `[:safe]` as options, as that will also enable the safety mechanisms
  # from `:erlang.binary_to_term/2` itself.
  # Ripped from https://github.com/elixir-plug/plug_crypto/blob/v1.2.0/lib/plug/crypto.ex

  # sobelow_skip ["Misc.BinToTerm"]
  def non_executable_binary_to_term(binary, opts \\ []) when is_binary(binary) do
    term = :erlang.binary_to_term(binary, opts)
    non_executable_terms(term)
    term
  end

  defp non_executable_terms(list) when is_list(list) do
    non_executable_list(list)
  end

  defp non_executable_terms(tuple) when is_tuple(tuple) do
    non_executable_tuple(tuple, tuple_size(tuple))
  end

  defp non_executable_terms(map) when is_map(map) do
    folder = fn key, value, acc ->
      non_executable_terms(key)
      non_executable_terms(value)
      acc
    end

    :maps.fold(folder, map, map)
  end

  defp non_executable_terms(other)
       when is_atom(other) or is_number(other) or is_bitstring(other) or is_pid(other) or
              is_reference(other) do
    other
  end

  defp non_executable_terms(other) do
    raise ArgumentError,
          "cannot deserialize #{inspect(other)}, the term is not safe for deserialization"
  end

  defp non_executable_list([]), do: :ok

  defp non_executable_list([h | t]) when is_list(t) do
    non_executable_terms(h)
    non_executable_list(t)
  end

  defp non_executable_list([h | t]) do
    non_executable_terms(h)
    non_executable_terms(t)
  end

  defp non_executable_tuple(_tuple, 0), do: :ok

  defp non_executable_tuple(tuple, n) do
    non_executable_terms(:erlang.element(n, tuple))
    non_executable_tuple(tuple, n - 1)
  end

  @doc false
  def deep_merge_maps(left, right) when is_map(left) and is_map(right) do
    Map.merge(left, right, fn _, left, right ->
      deep_merge_maps(left, right)
    end)
  end

  def deep_merge_maps(_left, right), do: right
end
