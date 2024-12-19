# Localized DateTime

## Introduction
Most datetimes will be recorded at UTC. This is useful, but we are often in a specific timezone that's not UTC and we'd like to see the date/time in our current locale. This is a perfect usecase for an `Ash.Calculation`.

```elixir
defmodule App.Calculations.LocalDateTime do
  use Ash.Resource.Calculation

  @impl true
  def init(opts), do: {:ok, opts}

  @impl true
  def load(_query, _opt, _context), do: []

  @impl true
  def calculate(records, opts, _context) do
    Enum.map(records, fn record ->
      record
      |> Map.get(opts[:field])
      |> Timex.to_datetime("Asia/Hong_Kong")
    end)
  end

  @impl true
  def expression(opts, _context) do
    expr(fragment("((? AT TIME ZONE 'UTC') AT TIME ZONE 'Asia/Hong_Kong')", ^ref(opts[:field])))
  end
end
```

The above code can be adjusted to a passed in value or even read the value from the context, but this is only an example to show how you can implement this calculation in both the data query and Elixir code.