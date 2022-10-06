defmodule Ash.Test.Flow.Steps.CountValue do
  @moduledoc false
  use Ash.Flow.Step

  def run(input, opts, _context) do
    field = opts[:field] || :value

    {:ok, input |> Map.get(field) |> List.wrap() |> Enum.count()}
  end
end
