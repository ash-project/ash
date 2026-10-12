# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Type.Range do
  @inner_types [:date, :integer, :naive_datetime, :datetime]
  @inner_type_modules [Ash.Type.Date, Ash.Type.Integer, Ash.Type.NaiveDatetime, Ash.Type.DateTime]
  @day 86_400_000_000

  @constraints [
    inner_type: [
      type: {:custom, __MODULE__, :validate_inner_type, []},
      required: true,
      doc: """
      The type of the range's values: one of `:integer`, `:date`, `:naive_datetime` or `:datetime`, or an `Ash.Type.NewType` of one, such as `:utc_datetime_usec`. Values are ordered the same way in Elixir and in the data layer. Each has a precision: see Inner types, above.
      """
    ],
    inner_constraints: [
      type: :keyword_list,
      default: [],
      doc: "Constraints of the inner type, applied to each bound, limit and anchor."
    ],
    lower: [
      type: :keyword_list,
      default: [],
      keys: [
        required?: [
          type: :boolean,
          default: false,
          doc: "The range must not be unbounded below."
        ],
        inclusive?: [
          type: :boolean,
          doc: """
          `true` requires an inclusive lower bound, `false` an exclusive one. A range unbounded below passes either. An `:integer` or `:date` range, or one with a `resolution`, is cast to `[)`, so only `true` is allowed.
          """
        ],
        limit: [
          type: :any,
          doc: """
          The least value the lower bound may take: `limit: ~D[2025-07-01]` keeps a range from starting before 1 July 2025. The range cannot then be unbounded below. Written as the lower bound, `:start` is this limit, or unbounded without one, so `[:start, :end)` is every value between the limits. With a `resolution`, it must be on the grid.
          """
        ]
      ],
      doc:
        "Constraints on the range's lower bound, which is `nil` when the range is unbounded below."
    ],
    upper: [
      type: :keyword_list,
      default: [],
      keys: [
        required?: [
          type: :boolean,
          default: false,
          doc: "The range must not be unbounded above."
        ],
        inclusive?: [
          type: :boolean,
          doc: """
          `true` requires an inclusive upper bound, `false` an exclusive one. A range unbounded above passes either. An `:integer` or `:date` range, or one with a `resolution`, is cast to `[)`, so only `false` is allowed.
          """
        ],
        limit: [
          type: :any,
          doc: """
          The greatest value the upper bound may take: `limit: ~D[2026-07-01]` keeps a range from ending after 1 July 2026. An inclusive upper bound must be below it. The range cannot then be unbounded above. Written as the upper bound, `:end` is this limit, or unbounded without one. With a `resolution`, it must be on the grid.
          """
        ]
      ],
      doc:
        "Constraints on the range's upper bound, which is `nil` when the range is unbounded above."
    ],
    allow_empty?: [
      type: :boolean,
      default: false,
      doc: "Allow an empty range, one holding no values (see `Ash.Range.empty/0`)."
    ],
    resolution: [
      type: :any,
      doc: """
      The length of a period: a positive `Duration`, or a positive integer for an `:integer` range. `Duration.new!(week: 2)` divides a `:date` range into fortnights. It is a whole number of the precision, in months and years or in weeks and shorter, not both.

      The periods' boundaries are the grid, and the range's bounds and limits must be on it. A range with a resolution is cast to `[)`. Without one, a period is one unit of the precision.
      """
    ],
    anchor: [
      type: :any,
      doc: """
      A value on the grid, aligning the periods, within the limits or not: `~D[2025-06-23]` starts fortnights on that Monday. Without one, the grid starts at `0` or at `0001-01-01T00:00:00Z`, so weeks start on Monday. Required for a resolution in months or years, on the 28th of a month or earlier.

      A `DateTime` in a time zone aligns periods of a day or longer to its time of day in that zone, so a day is 23, 24 or 25 hours long. Its time of day must fall in no gap or overlap of the zone.
      """
    ]
  ]

  @moduledoc """
  A range of values of an inner type, held as an `Ash.Range`.

      attribute :stay, Ash.Type.Range, constraints: [inner_type: :date]

  ### Inner types

  | inner type | precision | `resolution` | without a `resolution` |
  | --- | --- | --- | --- |
  | `:integer` | `1` | a positive integer | cast to `[)` |
  | `:date` | a day | a `Duration` of days or longer | cast to `[)` |
  | `:naive_datetime` | a second | a `Duration` | kept as given |
  | `:datetime` | a second, or a microsecond with `precision: :microsecond` | a `Duration` | kept as given |

  The precision is the smallest difference between two values. Cast to inclusive-exclusive,
  `[)`, `[1, 3]` becomes `[1, 4)`; kept as given, `(a, b]` stays `(a, b]`. With a
  `resolution`, every range is cast to `[)`.

  ### Constraints

  #{Spark.Options.docs(@constraints)}

  ### PostgreSQL

  `ash_postgres` stores a range as a native range: `:integer` as `int8range`, `:date` as
  `daterange`, `:naive_datetime` as `tsrange` and `:datetime` as `tstzrange`.
  """

  use Ash.Type

  alias Ash.Range

  @impl true
  def constraints, do: @constraints

  @impl true
  # A temporal period is generated from `as_of` by `Ash.Generator`, not here.
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

  @doc "The inner type, looking through any `Ash.Type.NewType`."
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
  Returns the period containing `value`. Takes constraints from `Ash.Type.init/2`.

      iex> {:ok, constraints} =
      ...>   Ash.Type.init(Ash.Type.Range,
      ...>     inner_type: :date,
      ...>     resolution: Duration.new!(week: 2),
      ...>     anchor: ~D[2025-06-23]
      ...>   )
      iex> Ash.Type.Range.period(~D[2025-06-25], constraints)
      {:ok, %Ash.Range{lower: ~D[2025-06-23], upper: ~D[2025-07-07], bounds: :"[)"}}
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
  Returns the length of a period: the `resolution`, or one unit of the precision without
  one. Takes constraints from `Ash.Type.init/2`.

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
         :ok <- limits_on_grid(constraints),
         :ok <- inclusivity_possible(constraints) do
      {:ok, constraints}
    end
  end

  # A discrete range is cast to `[)` before its bounds are checked, so a bounded lower is
  # always inclusive and a bounded upper always exclusive.
  defp inclusivity_possible(constraints) do
    cond do
      not discrete?(constraints) ->
        :ok

      constraints[:lower][:inclusive?] == false ->
        {:error,
         "`lower: [inclusive?: false]` would refuse every range with a lower bound: a range of integers or dates, or with a resolution, is cast to `[)`, so its lower bound is inclusive"}

      constraints[:upper][:inclusive?] == true ->
        {:error,
         "`upper: [inclusive?: true]` would refuse every range with an upper bound: a range of integers or dates, or with a resolution, is cast to `[)`, so its upper bound is exclusive"}

      true ->
        :ok
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
        kept = if zoned?(anchor, cast), do: anchor, else: cast

        cond do
          not (zoned?(anchor, cast) or held_as_given?(anchor, cast)) ->
            {:error,
             "the anchor #{inspect(anchor)} is not a value of the inner type as it holds it, #{inspect(cast)}"}

          calendar?(resolution) and kept.day > 28 ->
            {:error,
             "an anchor for a resolution in months or years falls on the 28th of a month or earlier, got: #{inspect(anchor)}"}

          true ->
            on_its_wall_clock(kept, resolution)
        end

      _ ->
        {:error, "the anchor #{inspect(anchor)} is not a value of the inner type"}
    end
  end

  # A zoned anchor is kept with its zone, where the time zone database places it as the same instant.
  defp zoned?(%DateTime{time_zone: zone} = given, %DateTime{} = cast) when zone != "Etc/UTC" do
    DateTime.compare(given, cast) == :eq and
      match?({:ok, _}, DateTime.shift_zone(cast, zone))
  end

  defp zoned?(_given, _cast), do: false

  # On a zone's wall clock a day is not a fixed length, and a time of day in a gap or an
  # overlap would skip or repeat a period.
  defp on_its_wall_clock(%DateTime{time_zone: zone} = anchor, resolution)
       when zone != "Etc/UTC" do
    cond do
      not wall?(resolution) ->
        {:ok, anchor}

      not calendar?(resolution) and rem(microseconds(resolution), @day) != 0 ->
        {:error,
         "a resolution of a day or longer on an anchor in #{zone} is a whole number of days, got: #{inspect(resolution)}"}

      time = time_in_a_transition(anchor) ->
        {:error, "the anchor's time of day, #{time}, falls in a gap or an overlap of #{zone}"}

      true ->
        {:ok, anchor}
    end
  end

  defp on_its_wall_clock(anchor, _resolution), do: {:ok, anchor}

  # The zone's rules for a year either side of the anchor.
  defp time_in_a_transition(%DateTime{time_zone: zone} = anchor) do
    time = DateTime.to_time(anchor)
    date = DateTime.to_date(anchor)

    if Enum.any?(-366..366, fn days ->
         not match?({:ok, _}, DateTime.new(Date.add(date, days), time, zone))
       end),
       do: time
  end

  # An anchor aligns the grid, so its cast must not move it.
  defp held_as_given?(%DateTime{} = given, %DateTime{} = cast),
    do: given.time_zone == cast.time_zone and DateTime.compare(given, cast) == :eq

  defp held_as_given?(%module{} = given, %module{} = cast), do: Comp.equal?(given, cast)

  defp held_as_given?(%_{}, _cast), do: false
  defp held_as_given?(_given, _cast), do: true

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
  # The data layer chooses the native range type.
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
      # The cast to `[)` can empty a range.
      if range.empty?, do: do_apply_constraints(range, constraints), else: {:ok, range}
    end
  end

  # An unbounded end passes any limit, so a limit requires a bound.
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

  # Between the limits is `[lower limit, upper limit)`.
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

  # Every empty range casts to `empty/0`, as in PostgreSQL; an inverted range is invalid.
  defp canonicalize(%Range{empty?: true}, _constraints), do: Range.empty()

  # The cast to `[)` can hide or create emptiness, so both forms are tested.
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

  # A discrete range is cast to `[)`; a continuous range is kept as written.
  defp discrete_bounds(%Range{lower: lower, upper: upper} = range, constraints)
       when not is_nil(lower) and not is_nil(upper) do
    # Shifting an inverted range would answer a cast with one it did not describe.
    if Comp.less_than?(upper, lower), do: range, else: shift_bounds(range, constraints)
  end

  defp discrete_bounds(%Range{} = range, constraints), do: shift_bounds(range, constraints)

  # Integers and dates are discrete, and so is any range with a resolution.
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
    step = constraints[:resolution] || precision(constraints)

    case wall_zone(constraints) do
      nil -> step_on(value, step)
      zone -> value |> on_wall(zone) |> step_on(step) |> from_wall(zone)
    end
    |> normalise(constraints)
  end

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

  # Without an anchor, the grid starts at 0 or 0001-01-01.
  defp anchor(constraints) do
    constraints[:anchor] ||
      case base_type(constraints[:inner_type]) do
        Ash.Type.Integer -> 0
        Ash.Type.Date -> ~D[0001-01-01]
        Ash.Type.DateTime -> ~U[0001-01-01 00:00:00Z]
        Ash.Type.NaiveDatetime -> ~N[0001-01-01 00:00:00]
      end
  end

  # Counted from the anchor, not from each other, so a month grid keeps its day.
  defp floor_to_grid(value, constraints) do
    resolution = constraints[:resolution]
    anchor = anchor(constraints)

    point =
      cond do
        zone = wall_zone(constraints) ->
          floor_on_wall(value, constraints, zone)

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

  # A zoned anchor's grid of a day or longer counts on the zone's wall clock.
  defp wall_zone(constraints) do
    case {constraints[:anchor], constraints[:resolution]} do
      {%DateTime{time_zone: zone}, %Duration{} = resolution} when zone != "Etc/UTC" ->
        if wall?(resolution), do: zone

      _ ->
        nil
    end
  end

  defp wall?(%Duration{} = resolution),
    do: calendar?(resolution) or microseconds(resolution) >= @day

  defp wall?(_resolution), do: false

  defp floor_on_wall(value, constraints, zone) do
    resolution = constraints[:resolution]
    anchor = DateTime.to_naive(constraints[:anchor])
    wall = on_wall(value, zone)

    point =
      if calendar?(resolution) do
        step = months(resolution)
        shift_months(anchor, Integer.floor_div(months_since(anchor, wall), step) * step)
      else
        step = div(microseconds(resolution), @day)
        NaiveDateTime.add(anchor, Integer.floor_div(days_since(anchor, wall), step) * step, :day)
      end

    from_wall(point, zone)
  end

  defp days_since(anchor, wall) do
    days = Date.diff(NaiveDateTime.to_date(wall), NaiveDateTime.to_date(anchor))

    if Comp.greater_than?(NaiveDateTime.add(anchor, days, :day), wall),
      do: days - 1,
      else: days
  end

  defp on_wall(%DateTime{} = value, zone),
    do: value |> DateTime.shift_zone!(zone) |> DateTime.to_naive()

  # Only a change to the zone's rules can put a grid point in a gap or an overlap.
  defp from_wall(wall, zone) do
    case DateTime.from_naive(wall, zone) do
      {:ok, datetime} -> datetime
      {:ambiguous, first, _second} -> first
      {:gap, _just_before, just_after} -> just_after
    end
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
