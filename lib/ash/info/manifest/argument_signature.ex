# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Info.Manifest.ArgumentSignature do
  @moduledoc """
  Normalized argument signature for an operator, function, or custom expression.

  One signature represents one accepted argument shape. The `args` list is the
  ordered list of arg specs; each arg spec has a `kind`:

    * `:concrete` — a specific type (`builtin` atom or `type_ref` module).
    * `:same` — same type as the field being filtered (or the first arg).
    * `:any` — any type accepted.
    * `:array` — an array whose inner element spec is in `of`.
    * `:ref` — fallback for shapes we don't structurally model yet.

  Use `from_ash_signature/1` to normalize the raw shape returned by an Ash
  operator's `types/0` or a function's `args/0` callback.
  """

  @type arg_spec :: %{
          required(:kind) => :concrete | :same | :any | :array | :ref,
          required(:builtin) => atom() | nil,
          required(:type_ref) => module() | nil,
          required(:constraints) => keyword(),
          optional(:of) => arg_spec()
        }

  @type t :: %__MODULE__{args: [arg_spec()]}

  defstruct args: []

  @doc """
  Normalize a raw signature from an Ash operator/function/custom expression callback.

  Accepts:
  - A list of arg specs (one explicit signature): `[:string, :string]`
  - A bare sentinel atom (`:same` or `:any`) treated as a one-arg signature.
  """
  @spec from_ash_signature(term()) :: t()
  def from_ash_signature(args) when is_list(args) do
    %__MODULE__{args: Enum.map(args, &normalize_arg/1)}
  end

  def from_ash_signature(atom) when atom in [:same, :any] do
    %__MODULE__{args: [normalize_arg(atom)]}
  end

  @doc "Normalize a single arg spec entry."
  @spec normalize_arg(term()) :: arg_spec()
  def normalize_arg(:same), do: spec(:same)
  def normalize_arg(:any), do: spec(:any)

  def normalize_arg(atom) when is_atom(atom) do
    if builtin_type?(atom) do
      spec(:concrete, builtin: atom)
    else
      spec(:concrete, type_ref: atom)
    end
  end

  def normalize_arg({module, constraints}) when is_atom(module) and is_list(constraints) do
    spec(:concrete, type_ref: module, constraints: constraints)
  end

  def normalize_arg({:array, inner}), do: spec(:array, of: normalize_arg(inner))
  def normalize_arg(_other), do: spec(:ref)

  defp spec(kind, opts \\ []) do
    base = %{
      kind: kind,
      builtin: Keyword.get(opts, :builtin),
      type_ref: Keyword.get(opts, :type_ref),
      constraints: Keyword.get(opts, :constraints, [])
    }

    case Keyword.fetch(opts, :of) do
      {:ok, of} -> Map.put(base, :of, of)
      :error -> base
    end
  end

  # Heuristic: lowercase, non-Elixir atom is a builtin (`:string`, `:integer`, ...).
  # Modules start with uppercase (`Ash.Type.Integer`).
  defp builtin_type?(atom) do
    case Atom.to_string(atom) do
      "Elixir." <> _ -> false
      _ -> true
    end
  end
end
