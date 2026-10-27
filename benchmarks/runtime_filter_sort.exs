# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

# Filtering and sorting records in memory, as `Ash.Query.apply_to/3` does.
#
# To compare a branch with `main`, run this on `main` with `SAVE=main`, then on the
# branch with `LOAD=main`.

defmodule Domain do
  use Ash.Domain, validate_config_inclusion?: false

  resources do
    allow_unregistered? true
  end
end

defmodule ComparableTimestamp do
  use Ash.Type.NewType, subtype_of: :utc_datetime_usec

  @impl true
  def operator_overloads do
    overloads =
      for other <- [Ash.Type.UtcDatetimeUsec, Ash.Type.UtcDatetime, Ash.Type.DateTime],
          signature <- [[__MODULE__, other], [other, __MODULE__]],
          into: %{[__MODULE__, __MODULE__] => Ash.Type.Boolean} do
        {signature, {[__MODULE__, __MODULE__], Ash.Type.Boolean}}
      end

    Map.new([:<, :<=, :>, :>=], &{&1, overloads})
  end
end

defmodule Points do
  use Ash.Type.NewType, subtype_of: :integer

  @impl true
  def operator_overloads, do: %{:+ => %{[__MODULE__, __MODULE__] => __MODULE__}}

  @impl true
  def evaluate_operator(%Ash.Query.Operator.Basic.Plus{left: left, right: right}),
    do: {:known, left + right}
end

plain_types =
  for name <- [Label, Code, Slug, Note, Tag, Handle] do
    {:module, module, _, _} =
      defmodule Module.concat(PlainType, name) do
        use Ash.Type.NewType, subtype_of: :string
      end

    module
  end

defmodule Reading do
  use Ash.Resource,
    data_layer: Ash.DataLayer.Ets,
    domain: Domain

  actions do
    defaults [:read, create: :*]
  end

  attributes do
    uuid_primary_key :id
    attribute :site, :string, public?: true
    attribute :sensor, :string, public?: true
    attribute :value, :decimal, public?: true
    attribute :level, :integer, public?: true
    attribute :at, :utc_datetime_usec, public?: true
  end
end

require Ash.Expr
require Ash.Query

now = ~U[2026-01-01 12:00:00.000000Z]
sites = ["north", "south", "east", "west"]
sensors = Enum.map(1..20, &"sensor-#{&1}")

readings = fn count, timestamps ->
  1..count
  |> Enum.map(fn i ->
    %{
      site: Enum.at(sites, rem(i, 4)),
      sensor: if(rem(i, 10) != 0, do: Enum.at(sensors, rem(i, 20))),
      value: Decimal.new(i - 50),
      level: rem(i, 13),
      at: DateTime.add(now, -rem(i * 7919, timestamps) * 60, :second)
    }
  end)
  |> Ash.bulk_create!(Reading, :create, return_records?: true)
  |> Map.fetch!(:records)
end

filter =
  Ash.Expr.expr(
    site in ^Enum.take(sites, 3) and sensor in ^Enum.take(sensors, 15) and value >= 0 and
      value < 400 and at >= ^DateTime.add(now, -50 * 60, :second) and at <= ^now
  )

arithmetic = Ash.Expr.expr(level + level > 10)

query =
  Reading
  |> Ash.Query.do_filter(filter)
  |> Ash.Query.sort(at: :desc)
  |> Ash.Query.limit(12)

options = fn suite ->
  [warmup: 1, time: 3, memory_time: 0.5, reduction_time: 0.5, title: suite] ++
    if(tag = System.get_env("SAVE"),
      do: [save: [path: Path.join(System.tmp_dir!(), "ash-#{suite}-#{tag}.benchee"), tag: tag]],
      else: []
    ) ++
    if(tag = System.get_env("LOAD"),
      do: [load: Path.join(System.tmp_dir!(), "ash-#{suite}-#{tag}.benchee")],
      else: []
    )
end

Logger.configure(level: :error)

known_types = [
  {"no known types", []},
  {"comparison overloads", [ComparableTimestamp]},
  {"8 known types", [ComparableTimestamp, Points | plain_types]}
]

Benchee.run(
  %{
    "filter_matches" => fn {records, _} ->
      {:ok, _} = Ash.Filter.Runtime.filter_matches(Domain, records, filter)
    end,
    "filter_matches, arithmetic" => fn {records, _} ->
      {:ok, _} = Ash.Filter.Runtime.filter_matches(Domain, records, arithmetic)
    end,
    "apply_to" => fn {records, _} -> {:ok, _} = Ash.Query.apply_to(query, records) end
  },
  [
    inputs:
      for count <- [100, 1000], {label, types} <- known_types, into: %{} do
        {"#{count} records, #{label}", {readings.(count, div(count, 5)), types}}
      end,
    before_scenario: fn {_, types} = input ->
      Application.put_env(:ash, :known_types, types)
      input
    end
  ] ++ options.("filter")
)

Application.put_env(:ash, :known_types, [])

sort = fn records, sort -> Ash.Actions.Sort.runtime_sort(records, sort, domain: Domain) end

Benchee.run(
  %{
    "one key" => fn {records, _} -> sort.(records, at: :desc) end,
    "two keys" => fn {records, _} -> sort.(records, at: :desc, id: :asc) end,
    "repeated keys" => fn {records, _} ->
      sort.(records, at: :desc, at: :desc, id: :asc, at: :desc)
    end,
    "one key, not held" => fn {_, not_held} -> sort.(not_held, at: :desc) end
  },
  [
    inputs:
      for count <- [100, 1000],
          {label, timestamps} <- [{"5 per timestamp", div(count, 5)}, {"unique", count}],
          into: %{} do
        records = readings.(count, timestamps)
        not_held = Enum.map(records, &%{&1 | at: %Ash.NotLoaded{field: :at, type: :attribute}})
        {"#{count} records, #{label}", {records, not_held}}
      end
  ] ++ options.("sort")
)
