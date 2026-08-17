# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Type.Range do
  @inner_types [:date, :integer, :naive_datetime, :datetime]

  @constraints [
    inner_type: [
      type: {:one_of, @inner_types},
      required: true,
      doc: "The type of the range's bounds. One of #{inspect(@inner_types)}."
    ],
    inner_constraints: [
      type: :keyword_list,
      default: [],
      doc: "Constraints applied to each bound, passed through to the inner type."
    ]
  ]

  @moduledoc """
  A continuous range of values of an inner type — the value type for temporal
  period columns (e.g `valid_at`).

  Parametrized by its inner type via constraints (one of `:date`, `:integer`,
  `:naive_datetime`, `:datetime`):

      attribute :valid_at, Ash.Type.Range, constraints: [inner_type: :datetime]

  Casts to/from an `Ash.Range` struct. The data layer maps it to a native range
  type — `ash_postgres` renders `:datetime` as `tstzrange`, `:date` as
  `daterange`, `:naive_datetime` as `tsrange`, `:integer` as `int8range`.

  ### Constraints

  #{Spark.Options.docs(@constraints)}
  """

  use Ash.Type

  alias Ash.Range

  @impl true
  def constraints, do: @constraints

  @impl true
  # Generate a non-empty `[)` range by drawing two values of the inner type and
  # ordering them. (Note: for a resource's temporal *period* attribute, generators
  # skip this and derive the period from `as_of` instead — see `Ash.Generator`.)
  def generator(constraints) do
    inner = Ash.Type.generator(constraints[:inner_type], constraints[:inner_constraints] || [])

    StreamData.bind(inner, fn a ->
      StreamData.map(inner, fn b ->
        {lower, upper} = if bound_lte?(a, b), do: {a, b}, else: {b, a}
        %Range{lower: lower, upper: upper, bounds: :"[)"}
      end)
    end)
  end

  defp bound_lte?(%Date{} = a, b), do: Date.compare(a, b) != :gt
  defp bound_lte?(%DateTime{} = a, b), do: DateTime.compare(a, b) != :gt
  defp bound_lte?(%NaiveDateTime{} = a, b), do: NaiveDateTime.compare(a, b) != :gt
  defp bound_lte?(a, b), do: a <= b

  @impl true
  def init(constraints) do
    type = Ash.Type.get_type(constraints[:inner_type])

    case Ash.Type.init(type, constraints[:inner_constraints] || []) do
      {:ok, inner_constraints} ->
        {:ok,
         constraints
         |> Keyword.put(:inner_type, type)
         |> Keyword.put(:inner_constraints, inner_constraints)}

      {:error, error} ->
        {:error, error}
    end
  end

  @impl true
  # Logical storage type. The concrete native range type (e.g. Postgres
  # `tstzrange`/`daterange`) is chosen by the data layer (see
  # `AshPostgres.SqlImplementation`/migration generator), not core.
  def storage_type(_constraints), do: :range

  @impl true
  def referenced_types(constraints) do
    type = Ash.Type.get_type(constraints[:inner_type])
    [{type, constraints[:inner_constraints] || [], {:inner_type_of, :range}}]
  end

  @impl true
  def matches_type?(%Range{}, _constraints), do: true
  def matches_type?(_, _constraints), do: false

  @impl true
  def cast_input(nil, _constraints), do: {:ok, nil}

  def cast_input(value, constraints) do
    with {:ok, lower, upper, bounds} <- extract(value),
         {:ok, lower} <- cast_bound(lower, :cast_input, constraints),
         {:ok, upper} <- cast_bound(upper, :cast_input, constraints) do
      {:ok, %Range{lower: lower, upper: upper, bounds: bounds}}
    end
  end

  @impl true
  def cast_stored(nil, _constraints), do: {:ok, nil}

  def cast_stored(value, constraints) do
    with {:ok, lower, upper, bounds} <- extract(value),
         {:ok, lower} <- cast_bound(lower, :cast_stored, constraints),
         {:ok, upper} <- cast_bound(upper, :cast_stored, constraints) do
      {:ok, %Range{lower: lower, upper: upper, bounds: bounds}}
    end
  end

  @impl true
  def dump_to_native(nil, _constraints), do: {:ok, nil}

  def dump_to_native(%Range{lower: lower, upper: upper, bounds: bounds}, constraints) do
    with {:ok, lower} <- dump_bound(lower, constraints),
         {:ok, upper} <- dump_bound(upper, constraints) do
      {:ok, %Range{lower: lower, upper: upper, bounds: bounds}}
    end
  end

  def dump_to_native(_, _constraints), do: :error

  @impl true
  def apply_constraints(nil, _constraints), do: {:ok, nil}

  def apply_constraints(%Range{lower: lower, upper: upper} = range, constraints) do
    type = constraints[:inner_type]
    inner = constraints[:inner_constraints] || []

    with {:ok, lower} <- apply_bound(type, lower, inner),
         {:ok, upper} <- apply_bound(type, upper, inner),
         :ok <- check_order(lower, upper) do
      {:ok, %{range | lower: lower, upper: upper}}
    end
  end

  defp check_order(nil, _), do: :ok
  defp check_order(_, nil), do: :ok

  defp check_order(lower, upper) do
    if compare(lower, upper) in [:lt, :eq] do
      :ok
    else
      {:error, message: "range lower bound must not be greater than upper bound"}
    end
  end

  # Best-effort ordering check across the bound types we support.
  defp compare(%struct{} = lower, upper) when struct in [DateTime, Date, NaiveDateTime] do
    struct.compare(lower, upper)
  end

  defp compare(lower, upper) when lower < upper, do: :lt
  defp compare(lower, upper) when lower > upper, do: :gt
  defp compare(_, _), do: :eq

  defp apply_bound(_type, nil, _inner), do: {:ok, nil}

  defp apply_bound(type, value, inner) do
    case Ash.Type.apply_constraints(type, value, inner) do
      {:ok, value} -> {:ok, value}
      :ok -> {:ok, value}
      {:error, error} -> {:error, error}
    end
  end

  defp extract(%Range{lower: lower, upper: upper, bounds: bounds}) do
    {:ok, lower, upper, normalize_bounds(bounds)}
  end

  defp extract({lower, upper}), do: {:ok, lower, upper, :"[)"}

  defp extract(%{} = map) do
    lower = map[:lower] || map["lower"]
    upper = map[:upper] || map["upper"]
    bounds = map[:bounds] || map["bounds"] || :"[)"
    {:ok, lower, upper, normalize_bounds(bounds)}
  end

  defp extract(_), do: {:error, "is not a valid range"}

  defp normalize_bounds(bounds) when is_atom(bounds), do: bounds
  defp normalize_bounds(bounds) when is_binary(bounds), do: String.to_existing_atom(bounds)

  defp cast_bound(nil, _fun, _constraints), do: {:ok, nil}

  defp cast_bound(value, fun, constraints) do
    apply(Ash.Type, fun, [constraints[:inner_type], value, constraints[:inner_constraints] || []])
  end

  defp dump_bound(nil, _constraints), do: {:ok, nil}

  defp dump_bound(value, constraints) do
    Ash.Type.dump_to_native(
      constraints[:inner_type],
      value,
      constraints[:inner_constraints] || []
    )
  end
end
