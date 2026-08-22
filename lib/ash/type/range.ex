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
    ],
    lower: [
      type: :keyword_list,
      default: [],
      keys: [
        required?: [
          type: :boolean,
          default: false,
          doc: "The range must have a lower bound."
        ],
        inclusive?: [
          type: :boolean,
          doc: "The lower bound, where there is one, must include its own value."
        ]
      ],
      doc: "Constraints on the range's lower bound."
    ],
    upper: [
      type: :keyword_list,
      default: [],
      keys: [
        required?: [
          type: :boolean,
          default: false,
          doc: "The range must have an upper bound."
        ],
        inclusive?: [
          type: :boolean,
          doc: "The upper bound, where there is one, must include its own value."
        ]
      ],
      doc: "Constraints on the range's upper bound."
    ],
    allow_empty?: [
      type: :boolean,
      default: false,
      doc: "If false, a range containing no points is refused."
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
    with {:ok, lower, upper, bounds, empty?} <- extract(value),
         {:ok, lower} <- cast_bound(lower, :cast_input, constraints),
         {:ok, upper} <- cast_bound(upper, :cast_input, constraints) do
      {:ok,
       canonicalize(
         %Range{lower: lower, upper: upper, bounds: bounds, empty?: empty?},
         constraints
       )}
    end
  end

  @impl true
  def cast_stored(nil, _constraints), do: {:ok, nil}

  def cast_stored(value, constraints) do
    with {:ok, lower, upper, bounds, empty?} <- extract(value),
         {:ok, lower} <- cast_bound(lower, :cast_stored, constraints),
         {:ok, upper} <- cast_bound(upper, :cast_stored, constraints) do
      {:ok,
       canonicalize(
         %Range{lower: lower, upper: upper, bounds: bounds, empty?: empty?},
         constraints
       )}
    end
  end

  @impl true
  def dump_to_native(nil, _constraints), do: {:ok, nil}
  def dump_to_native(%Range{empty?: true}, _constraints), do: {:ok, Range.empty()}

  def dump_to_native(%Range{lower: lower, upper: upper, bounds: bounds}, constraints) do
    if Range.valid_bounds?(bounds) do
      with {:ok, lower} <- dump_bound(lower, constraints),
           {:ok, upper} <- dump_bound(upper, constraints) do
        {:ok, %Range{lower: lower, upper: upper, bounds: bounds}}
      end
    else
      :error
    end
  end

  def dump_to_native(_, _constraints), do: :error

  @impl true
  def apply_constraints(nil, _constraints), do: {:ok, nil}

  def apply_constraints(%Range{bounds: bounds} = range, constraints) do
    if Range.valid_bounds?(bounds) do
      do_apply_constraints(range, constraints)
    else
      {:error, message: "range bounds must be a valid bounds specifier"}
    end
  end

  def apply_constraints(_value, _constraints), do: {:error, message: "is not a valid range"}

  # An empty range is constructed, not mistyped, so it is refused rather than nulled.
  defp do_apply_constraints(%Range{empty?: true} = range, constraints) do
    if Keyword.get(constraints, :allow_empty?, false) do
      {:ok, range}
    else
      {:error, message: "range must not be empty"}
    end
  end

  defp do_apply_constraints(%Range{lower: lower, upper: upper} = range, constraints) do
    type = constraints[:inner_type]
    inner = constraints[:inner_constraints] || []

    with {:ok, lower} <- apply_bound(type, lower, inner),
         {:ok, upper} <- apply_bound(type, upper, inner),
         :ok <- check_order(lower, upper),
         range = canonicalize(%{range | lower: lower, upper: upper}, constraints),
         :ok <- check_bound(:lower, range, constraints[:lower] || []),
         :ok <- check_bound(:upper, range, constraints[:upper] || []) do
      # Canonicalizing can empty a range, so the empty rule is applied to the result.
      if range.empty?, do: do_apply_constraints(range, constraints), else: {:ok, range}
    end
  end

  # Each end on its own terms: there if required, and of the asked-for inclusivity if
  # there. An absent end includes nothing, so only its presence can be constrained.
  defp check_bound(end_name, range, bound_constraints) do
    value = Map.fetch!(range, end_name)
    inclusive? = inclusive?(end_name, range.bounds)

    cond do
      is_nil(value) and Keyword.get(bound_constraints, :required?, false) ->
        {:error, message: "range must have a %{bound} bound", vars: [bound: end_name]}

      is_nil(value) ->
        :ok

      matches_inclusivity?(inclusive?, bound_constraints[:inclusive?]) ->
        :ok

      true ->
        {:error,
         message: "range %{bound} bound must be %{required}",
         vars: [bound: end_name, required: inclusivity_name(bound_constraints[:inclusive?])]}
    end
  end

  defp inclusive?(:lower, bounds), do: Range.lower_inclusive?(bounds)
  defp inclusive?(:upper, bounds), do: Range.upper_inclusive?(bounds)

  defp matches_inclusivity?(_actual, nil), do: true
  defp matches_inclusivity?(actual, required), do: actual == required

  defp inclusivity_name(true), do: "inclusive"
  defp inclusivity_name(false), do: "exclusive"

  # Every range containing no points is the same range, so they cast to one value with
  # no bounds, as Postgres does, letting a data layer that keeps no bounds for an empty
  # range read one back. An inverted range is invalid rather than empty, so it is left.
  defp canonicalize(%Range{empty?: true}, _constraints), do: Range.empty()

  # The discrete shift can both hide and create emptiness, so both sides are tested. An
  # inverted range is empty by neither, and falls through to check_order/2.
  defp canonicalize(%Range{} = range, constraints) do
    if empty_bounds?(range) do
      Range.empty()
    else
      shifted = discrete_bounds(range, constraints[:inner_type])

      if empty_bounds?(shifted), do: Range.empty(), else: shifted
    end
  end

  defp empty_bounds?(%Range{lower: lower, upper: upper, bounds: bounds})
       when not is_nil(lower) and not is_nil(upper) do
    Comp.equal?(lower, upper) and
      not (Range.lower_inclusive?(bounds) and Range.upper_inclusive?(bounds))
  end

  defp empty_bounds?(%Range{}), do: false

  # A discrete type has a successor, so every range over it has one `[)` spelling: an
  # exclusive lower and an inclusive upper each move on to the next value, and an
  # unbounded end is exclusive. A continuous type has none, so is left as written.
  defp discrete_bounds(%Range{lower: lower, upper: upper} = range, type)
       when type in [Ash.Type.Integer, Ash.Type.Date] and not is_nil(lower) and
              not is_nil(upper) do
    # Shifting an inverted range would answer a cast with one it did not describe.
    if Comp.less_than?(upper, lower), do: range, else: shift_bounds(range)
  end

  defp discrete_bounds(%Range{} = range, type) when type in [Ash.Type.Integer, Ash.Type.Date] do
    shift_bounds(range)
  end

  defp discrete_bounds(%Range{} = range, _type), do: range

  defp shift_bounds(%Range{} = range) do
    lower =
      if is_nil(range.lower) or Range.lower_inclusive?(range.bounds),
        do: range.lower,
        else: successor(range.lower)

    upper =
      if is_nil(range.upper) or not Range.upper_inclusive?(range.bounds),
        do: range.upper,
        else: successor(range.upper)

    bounds = if is_nil(lower), do: :"()", else: :"[)"

    %{range | lower: lower, upper: upper, bounds: bounds}
  end

  defp successor(value) when is_integer(value), do: value + 1
  defp successor(%Date{} = value), do: Date.add(value, 1)

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

  defp extract(%Range{lower: lower, upper: upper, bounds: bounds, empty?: empty?}) do
    with {:ok, bounds} <- normalize_bounds(bounds) do
      {:ok, lower, upper, bounds, empty?}
    end
  end

  defp extract({lower, upper}), do: {:ok, lower, upper, :"[)", false}

  defp extract(%{} = map) when not is_struct(map) do
    lower = map[:lower] || map["lower"]
    upper = map[:upper] || map["upper"]
    bounds = map[:bounds] || map["bounds"] || :"[)"
    empty? = map[:empty?] || map["empty?"] || false

    with {:ok, bounds} <- normalize_bounds(bounds) do
      {:ok, lower, upper, bounds, empty?}
    end
  end

  defp extract(_), do: {:error, "is not a valid range"}

  defp normalize_bounds(bounds) when is_atom(bounds) do
    if Range.valid_bounds?(bounds), do: {:ok, bounds}, else: bounds_error()
  end

  defp normalize_bounds(bounds) when is_binary(bounds) do
    normalize_bounds(String.to_existing_atom(bounds))
  rescue
    ArgumentError -> bounds_error()
  end

  defp normalize_bounds(_), do: bounds_error()

  defp bounds_error, do: {:error, "bounds is not a valid bounds specifier"}

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
