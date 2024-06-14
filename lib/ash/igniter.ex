defmodule Ash.Igniter do
  def csv_option(options, key, modifier \\ & &1) do
    Keyword.update(
      options,
      key,
      [],
      fn defaults ->
        defaults
        |> List.wrap()
        |> Enum.join(",")
        |> String.split(",")
        |> then(modifier)
      end
    )
  end
end
