# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Type.Range do
  @inner_types [:date, :integer, :naive_datetime, :datetime]
  @inner_type_modules [Ash.Type.Date, Ash.Type.Integer, Ash.Type.NaiveDatetime, Ash.Type.DateTime]

  @constraints [
    inner_type: [
      type: {:custom, __MODULE__, :validate_inner_type, []},
      required: true,
      doc:
        "The type of the range's bounds. One of #{inspect(@inner_types)}, or a `Ash.Type.NewType` of one of them, like `:utc_datetime_usec`."
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
        ],
        limit: [
          type: :any,
          doc: "The least value the range may start at. With a resolution, a period boundary."
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
        ],
        limit: [
          type: :any,
          doc: "The greatest value the range may end at. With a resolution, a period boundary."
        ]
      ],
      doc: "Constraints on the range's upper bound."
    ],
    allow_empty?: [
      type: :boolean,
      default: false,
      doc: "If false, a range containing no points is refused."
    ],
    resolution: [
      type: :any,
      doc:
        "The span of the range's periods: a positive `Duration`, or a positive integer for an integer range. It is a whole number of the inner type's precision, and is in months and years, or in weeks and finer, not both."
    ],
    anchor: [
      type: :any,
      doc:
        "A value aligning the resolution's periods, within the limits or not. Required for a resolution in months or years, and then on the 28th of a month or earlier."
    ]
  ]

  @moduledoc """
  A continuous range of values of an inner type — the value type for temporal
  period columns (e.g `valid_at`).

  Parametrized by its inner type via constraints (one of `:date`, `:integer`,
  `:naive_datetime`, `:datetime`):

      attribute :valid_at, Ash.Type.Range, constraints: [inner_type: :datetime]

  A range with a `resolution` has periods of that span, aligned by its `anchor`, within its
  limits or not. Its bounds and limits are period boundaries, and it casts to its one `[)`
  form, so `[a, b]` is `[a, b + resolution)`. Without an anchor below months, the grid runs from
  `0001-01-01T00:00:00Z`: minutes and hours follow the UTC clock, and weeks start on Monday.

      attribute :pay_period, Ash.Type.Range,
        constraints: [
          inner_type: :date,
          resolution: Duration.new!(week: 2),
          anchor: ~D[2025-06-23]
        ]

  A lower bound written `:start` is the range's lower limit, and an upper bound written
  `:end` its upper limit, each unbounded where the range has none.

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

  @doc false
  def validate_inner_type(type) do
    if base_type(type) in @inner_type_modules do
      {:ok, type}
    else
      {:error,
       "expected one of #{inspect(@inner_types)}, or a NewType of one of them, got: #{inspect(type)}"}
    end
  end

  @doc "The type a range's bounds are built from, looking through any `Ash.Type.NewType`."
  @spec base_type(Ash.Type.t()) :: Ash.Type.t()
  def base_type(type) do
    type = Ash.Type.get_type(type)

    if Ash.Type.NewType.new_type?(type) do
      Ash.Type.get_type(Ash.Type.NewType.subtype_of(type))
    else
      type
    end
  end

  @doc """
  The period of the range's resolution holding `value`: `[start, start + resolution)`.

  The value is cast by the inner type first. With a `resolution`, the period starts at the
  grid point at or before the value. Without one it starts at the value, and is one unit of
  the inner type's precision long: one for an integer, a day for a date, and for a datetime
  a second or a microsecond by its `precision`. Takes initialised constraints.

      iex> {:ok, constraints} = Ash.Type.init(Ash.Type.Range, inner_type: :date)
      iex> Ash.Type.Range.period(~D[2026-10-07], constraints)
      {:ok, %Ash.Range{lower: ~D[2026-10-07], upper: ~D[2026-10-08], bounds: :"[)"}}
  """
  @spec period(term(), Keyword.t()) :: {:ok, Range.t()} | {:error, String.t()}
  def period(value, constraints) do
    case cast_bound(value, :cast_input, constraints) do
      {:ok, value} when not is_nil(value) ->
        start = if constraints[:resolution], do: floor_to_grid(value, constraints), else: value
        {:ok, %Range{lower: start, upper: successor(start, constraints), bounds: :"[)"}}

      _ ->
        {:error, "is not a value of the range's inner type"}
    end
  end

  @doc """
  The span of the range's periods: its `resolution`, or one unit of the inner type's
  precision where it declares none. Takes initialised constraints.

      iex> {:ok, constraints} = Ash.Type.init(Ash.Type.Range, inner_type: :utc_datetime)
      iex> Ash.Type.Range.resolution(constraints)
      %Duration{second: 1}
  """
  @spec resolution(Keyword.t()) :: Duration.t() | pos_integer()
  def resolution(constraints) do
    constraints[:resolution] || precision(constraints)
  end

  @impl true
  def init(constraints) do
    type = Ash.Type.get_type(constraints[:inner_type])

    with {:ok, inner_constraints} <- Ash.Type.init(type, constraints[:inner_constraints] || []),
         {:ok, lower} <- init_limit(:lower, constraints[:lower], type, inner_constraints),
         {:ok, upper} <- init_limit(:upper, constraints[:upper], type, inner_constraints),
         constraints =
           constraints
           |> Keyword.put(:inner_type, type)
           |> Keyword.put(:inner_constraints, inner_constraints)
           |> put_present(:lower, lower)
           |> put_present(:upper, upper),
         {:ok, constraints} <- init_grid(constraints),
         :ok <- limits_on_grid(constraints) do
      {:ok, constraints}
    end
  end

  defp init_grid(constraints) do
    case {constraints[:resolution], constraints[:anchor]} do
      {nil, nil} ->
        {:ok, constraints}

      {nil, _anchor} ->
        {:error, "an anchor needs a resolution"}

      {resolution, anchor} ->
        base = base_type(constraints[:inner_type])

        with {:ok, resolution} <- cast_resolution(base, resolution),
             :ok <- whole_precision(resolution, constraints),
             {:ok, anchor} <- cast_anchor(anchor, resolution, constraints) do
          {:ok,
           constraints
           |> Keyword.put(:resolution, resolution)
           |> put_present(:anchor, anchor)}
        end
    end
  end

  defp cast_resolution(Ash.Type.Integer, resolution)
       when is_integer(resolution) and resolution > 0,
       do: {:ok, resolution}

  defp cast_resolution(Ash.Type.Integer, resolution),
    do:
      {:error,
       "the resolution of an integer range is a positive integer, got: #{inspect(resolution)}"}

  defp cast_resolution(_base, resolution) do
    with {:ok, %Duration{} = duration} <- to_duration(resolution),
         {:ok, duration} <-
           Ash.Type.apply_constraints(Ash.Type.Duration, duration, signs: [:positive]) do
      if months(duration) != 0 and microseconds(duration) != 0,
        do: {:error, "a resolution is in months and years, or in weeks and finer, not both"},
        else: {:ok, duration}
    else
      _ -> {:error, "the resolution is a positive duration, got: #{inspect(resolution)}"}
    end
  end

  defp to_duration(%Duration{} = duration), do: {:ok, duration}

  defp to_duration(value), do: Ash.Type.cast_input(Ash.Type.Duration, value, [])

  defp whole_precision(resolution, _constraints) when is_integer(resolution), do: :ok

  defp whole_precision(resolution, constraints) do
    if months(resolution) != 0 or
         rem(microseconds(resolution), microseconds(precision(constraints))) == 0,
       do: :ok,
       else:
         {:error,
          "the resolution #{inspect(resolution)} is not a whole number of the inner type's precision, #{inspect(precision(constraints))}"}
  end

  defp cast_anchor(nil, resolution, _constraints) do
    if calendar?(resolution),
      do: {:error, "a resolution in months or years needs an anchor"},
      else: {:ok, nil}
  end

  defp cast_anchor(anchor, resolution, constraints) do
    case cast_bound(anchor, :cast_input, constraints) do
      {:ok, cast} when not is_nil(cast) ->
        if calendar?(resolution) and cast.day > 28,
          do:
            {:error,
             "an anchor for a resolution in months or years falls on the 28th of a month or earlier, got: #{inspect(anchor)}"},
          else: {:ok, cast}

      _ ->
        {:error, "the anchor #{inspect(anchor)} is not a value of the inner type"}
    end
  end

  defp limits_on_grid(constraints) do
    Enum.find_value([:lower, :upper], :ok, fn end_name ->
      limit = constraints[end_name][:limit]

      if limit && not on_grid?(limit, constraints),
        do: {:error, "the #{end_name} limit #{inspect(limit)} is not on the resolution's grid"}
    end)
  end

  # A limit is cast by the inner type once, so it compares with bounds in their own form.
  defp init_limit(_end_name, nil, _type, _inner_constraints), do: {:ok, nil}

  defp init_limit(end_name, bound_constraints, type, inner_constraints) do
    case Keyword.fetch(bound_constraints, :limit) do
      {:ok, limit} when not is_nil(limit) ->
        case Ash.Type.cast_input(type, limit, inner_constraints) do
          {:ok, cast} when not is_nil(cast) ->
            {:ok, Keyword.put(bound_constraints, :limit, cast)}

          _ ->
            {:error, "the #{end_name} limit #{inspect(limit)} is not a value of the inner type"}
        end

      _ ->
        {:ok, bound_constraints}
    end
  end

  defp put_present(constraints, _key, nil), do: constraints
  defp put_present(constraints, key, value), do: Keyword.put(constraints, key, value)

  @impl true
  # Logical storage type. The concrete native range type (e.g. Postgres
  # `tstzrange`/`daterange`) is chosen by the data layer (see
  # `AshPostgres.SqlImplementation`/migration generator), not core.
  def storage_type(_constraints), do: :range

  @impl true
  def referenced_types(constraints) do
    case type_parameter(constraints) do
      {type, inner_constraints} -> [{type, inner_constraints, {:inner_type_of, :range}}]
      nil -> []
    end
  end

  @impl true
  def type_parameter(constraints) do
    case constraints[:inner_type] do
      nil -> nil
      inner_type -> {Ash.Type.get_type(inner_type), constraints[:inner_constraints] || []}
    end
  end

  @impl true
  def with_type_parameter(constraints, {inner_type, inner_constraints}) do
    inner_type = Ash.Type.get_type(inner_type)

    # `inner_type` is validated against short names, so write the short name back.
    inner_type =
      Enum.find_value(Ash.Type.short_names(), inner_type, fn {short_name, module} ->
        if module == inner_type, do: short_name
      end)

    constraints
    |> Keyword.put(:inner_type, inner_type)
    |> Keyword.put(:inner_constraints, inner_constraints)
  end

  @impl true
  def matches_type?(%Range{}, _constraints), do: true
  def matches_type?(_, _constraints), do: false

  @impl true
  def cast_input(nil, _constraints), do: {:ok, nil}

  def cast_input(value, constraints) do
    with {:ok, lower, upper, bounds, empty?} <- extract(value),
         {:ok, lower} <- resolve_limit(:lower, lower, constraints),
         {:ok, upper} <- resolve_limit(:upper, upper, constraints),
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
         :ok <- check_bound(:lower, range, constraints[:lower] || [], constraints),
         :ok <- check_bound(:upper, range, constraints[:upper] || [], constraints) do
      # Canonicalizing can empty a range, so the empty rule is applied to the result.
      if range.empty?, do: do_apply_constraints(range, constraints), else: {:ok, range}
    end
  end

  # Each end on its own terms: there if required or limited, of the asked-for inclusivity
  # if there, on the grid, and within its limit. An unbounded end runs past any limit.
  defp check_bound(end_name, range, bound_constraints, constraints) do
    value = Map.fetch!(range, end_name)
    inclusive? = inclusive?(end_name, range.bounds)
    limit = bound_constraints[:limit]

    cond do
      is_nil(value) and (Keyword.get(bound_constraints, :required?, false) or not is_nil(limit)) ->
        {:error, message: "range must have a %{bound} bound", vars: [bound: end_name]}

      is_nil(value) ->
        :ok

      not matches_inclusivity?(inclusive?, bound_constraints[:inclusive?]) ->
        {:error,
         message: "range %{bound} bound must be %{required}",
         vars: [bound: end_name, required: inclusivity_name(bound_constraints[:inclusive?])]}

      not on_grid?(value, constraints) ->
        {:error,
         message: "range %{bound} bound must be on the grid of its resolution, %{resolution}",
         vars: [bound: end_name, resolution: constraints[:resolution]]}

      not within_limit?(end_name, value, inclusive?, limit) ->
        {:error,
         message: "range %{bound} bound must be within its limit %{limit}",
         vars: [bound: end_name, limit: limit]}

      true ->
        :ok
    end
  end

  # The limits bound a `[)` window: from the lower limit, up to the upper limit excluded.
  defp within_limit?(_end_name, _value, _inclusive?, nil), do: true
  defp within_limit?(:lower, value, _inclusive?, limit), do: Comp.compare(value, limit) != :lt
  defp within_limit?(:upper, value, false, limit), do: Comp.compare(value, limit) != :gt
  defp within_limit?(:upper, value, true, limit), do: Comp.compare(value, limit) == :lt

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
      shifted = if discrete?(constraints), do: discrete_bounds(range, constraints), else: range

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
  defp discrete_bounds(%Range{lower: lower, upper: upper} = range, constraints)
       when not is_nil(lower) and not is_nil(upper) do
    # Shifting an inverted range would answer a cast with one it did not describe.
    if Comp.less_than?(upper, lower), do: range, else: shift_bounds(range, constraints)
  end

  defp discrete_bounds(%Range{} = range, constraints), do: shift_bounds(range, constraints)

  # Integers and dates are discrete by their precision; any inner type is on a resolution.
  defp discrete?(constraints) do
    not is_nil(constraints[:resolution]) or
      base_type(constraints[:inner_type]) in [Ash.Type.Integer, Ash.Type.Date]
  end

  defp shift_bounds(%Range{} = range, constraints) do
    lower =
      if is_nil(range.lower) or Range.lower_inclusive?(range.bounds),
        do: range.lower,
        else: successor(range.lower, constraints)

    upper =
      if is_nil(range.upper) or not Range.upper_inclusive?(range.bounds),
        do: range.upper,
        else: successor(range.upper, constraints)

    bounds = if is_nil(lower), do: :"()", else: :"[)"

    %{range | lower: lower, upper: upper, bounds: bounds}
  end

  defp successor(value, constraints) do
    value
    |> step_on(constraints[:resolution] || precision(constraints))
    |> normalise(constraints)
  end

  @day 86_400_000_000

  # Ash.Type.NaiveDatetime casts through Ecto's :naive_datetime, which truncates to the second.
  defp precision(constraints) do
    case base_type(constraints[:inner_type]) do
      Ash.Type.Integer ->
        1

      Ash.Type.Date ->
        Duration.new!(day: 1)

      Ash.Type.DateTime ->
        if constraints[:inner_constraints][:precision] == :microsecond,
          do: Duration.new!(microsecond: {1, 6}),
          else: Duration.new!(second: 1)

      Ash.Type.NaiveDatetime ->
        Duration.new!(second: 1)
    end
  end

  defp months(%Duration{year: year, month: month}), do: year * 12 + month

  defp microseconds(%Duration{microsecond: {microsecond, _}} = duration) do
    ((((duration.week * 7 + duration.day) * 24 + duration.hour) * 60 + duration.minute) * 60 +
       duration.second) * 1_000_000 + microsecond
  end

  defp calendar?(%Duration{} = resolution), do: months(resolution) != 0
  defp calendar?(_resolution), do: false

  defp step_on(value, step) when is_integer(step), do: value + step

  defp step_on(value, step) do
    if calendar?(step),
      do: shift_months(value, months(step)),
      else: add_microseconds(value, microseconds(step))
  end

  # Without an anchor, the grid runs from the start of the first year.
  defp anchor(constraints) do
    constraints[:anchor] ||
      case base_type(constraints[:inner_type]) do
        Ash.Type.Integer -> 0
        Ash.Type.Date -> ~D[0001-01-01]
        Ash.Type.DateTime -> ~U[0001-01-01 00:00:00Z]
        Ash.Type.NaiveDatetime -> ~N[0001-01-01 00:00:00]
      end
  end

  # Grid points are counted from the anchor, never stepped from each other, since a month
  # step from a month's end shortens the next.
  defp floor_to_grid(value, constraints) do
    resolution = constraints[:resolution]
    anchor = anchor(constraints)

    point =
      cond do
        is_integer(resolution) ->
          anchor + Integer.floor_div(value - anchor, resolution) * resolution

        calendar?(resolution) ->
          step = months(resolution)
          shift_months(anchor, Integer.floor_div(months_since(anchor, value), step) * step)

        true ->
          step = microseconds(resolution)

          add_microseconds(
            anchor,
            Integer.floor_div(microseconds_since(anchor, value), step) * step
          )
      end

    normalise(point, constraints)
  end

  defp on_grid?(value, constraints) do
    is_nil(constraints[:resolution]) or Comp.equal?(floor_to_grid(value, constraints), value)
  end

  defp months_since(anchor, value) do
    months = (value.year - anchor.year) * 12 + value.month - anchor.month

    if Comp.greater_than?(shift_months(anchor, months), value), do: months - 1, else: months
  end

  defp shift_months(%Date{} = value, months), do: Date.shift(value, month: months)
  defp shift_months(%DateTime{} = value, months), do: DateTime.shift(value, month: months)

  defp shift_months(%NaiveDateTime{} = value, months),
    do: NaiveDateTime.shift(value, month: months)

  defp microseconds_since(%Date{} = anchor, value), do: Date.diff(value, anchor) * @day

  defp microseconds_since(%DateTime{} = anchor, value),
    do: DateTime.diff(value, anchor, :microsecond)

  defp microseconds_since(%NaiveDateTime{} = anchor, value),
    do: NaiveDateTime.diff(value, anchor, :microsecond)

  defp add_microseconds(%Date{} = value, microseconds),
    do: Date.add(value, div(microseconds, @day))

  defp add_microseconds(%DateTime{} = value, microseconds),
    do: DateTime.add(value, microseconds, :microsecond)

  defp add_microseconds(%NaiveDateTime{} = value, microseconds),
    do: NaiveDateTime.add(value, microseconds, :microsecond)

  # Cast back by the inner type, so a computed value takes its precision.
  defp normalise(value, constraints) do
    case cast_bound(value, :cast_input, constraints) do
      {:ok, cast} -> cast
      _ -> value
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

  # `:start` and `:end` name the range's limits, and are unbounded where it has none.
  defp resolve_limit(:lower, :start, constraints), do: {:ok, constraints[:lower][:limit]}
  defp resolve_limit(:upper, :end, constraints), do: {:ok, constraints[:upper][:limit]}

  defp resolve_limit(end_name, limit, _constraints) when limit in [:start, :end],
    do: {:error, "#{inspect(limit)} cannot be the #{end_name} bound"}

  defp resolve_limit(_end_name, value, _constraints), do: {:ok, value}

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
