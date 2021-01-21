defmodule Ash do
  @moduledoc " Types and simple helpers for Ash"
  alias Ash.Resource.Actions.{Create, Destroy, Read, Update}
  alias Ash.Resource.Relationships.{BelongsTo, HasMany, HasOne, ManyToMany}

  @type action :: Create.t() | Read.t() | Update.t() | Destroy.t()
  @type action_type :: :read | :create | :update | :destroy
  @type actor :: Ash.record()
  @type aggregate :: Ash.Query.Aggregate.t() | Ash.Resource.Aggregate.t()
  @type aggregate_kind :: Ash.Query.Aggregate.kind()
  @type api :: module
  @type attribute :: Ash.Resource.Attribute.t()
  @type calculation :: Ash.Resource.Calculation.t()
  @type cardinality_many_relationship() :: HasMany.t() | ManyToMany.t()
  @type cardinality_one_relationship() :: HasOne.t() | BelongsTo.t()
  @type changeset :: Ash.Changeset.t()
  @type data_layer :: module
  @type data_layer_query :: struct
  @type error :: struct
  @type filter :: Ash.Filter.t()
  @type params :: Keyword.t()
  @type primary_key :: record() | map | term
  @type query :: Ash.Query.t()
  @type record :: struct
  @type relationship :: cardinality_one_relationship() | cardinality_many_relationship()
  @type relationship_cardinality :: :many | :one
  @type resource :: module
  @type side_loads :: term
  @type page :: Ash.Page.Keyset.t() | Ash.Page.Offset.t()
  @type sort_order ::
          :asc | :desc | :asc_nils_first | :asc_nils_last | :desc_nils_first | :desc_nils_last
  @type sort :: list(atom | {atom, sort_order})
  @type validation :: Ash.Resource.Validation.t()
  @type notification :: Ash.Notifier.Notification.t()

  require Ash.Dsl.Extension

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

  def uuid do
    Ecto.UUID.generate()
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
  def non_executable_binary_to_term(binary, opts) when is_binary(binary) do
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

  @doc "Returns all extensions of a resource or api"
  @spec extensions(resource() | api()) :: [module]
  def extensions(resource) do
    Ash.Dsl.Extension.get_persisted(resource, :extensions)
  end

  @spec try_compile(term) :: :ok
  def try_compile(module) when is_atom(module) do
    Code.ensure_loaded(module)
    :ok
  end

  def try_compile(_), do: :ok
end
