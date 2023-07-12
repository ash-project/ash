defmodule Ash.Query.Function.StringSplit do
  @options [
    trim?: [
      type: :boolean,
      default: false,
      doc:
        "Wether or not to trim empty strings from the beginning or end of the result. Equivalent to the `trim` option to `String.split/3`"
    ]
  ]

  @moduledoc """
  Split a string into a list of strings

  Splits a string on the given delimiter. The delimiter defaults to a single space. Also supports options.

  Keep in mind, this function does *not* support regexes the way that `String.split/3` does, only raw strings.

      string_split(employee_code)
      string_split(full_name, "foo")
      string_split(full_name, "foo", trim?: true)

  ## Options

  #{Spark.OptionsHelpers.docs(@options)}
  """

  use Ash.Query.Function, name: :string_split

  def args,
    do: [
      [:string],
      [:string, :string],
      [:string, :string, {:keyword, fields: @options}]
    ]

  def new([string]) do
    {:ok, %__MODULE__{arguments: [string, " ", []]}}
  end

  def new([string, delimiter]) do
    {:ok, %__MODULE__{arguments: [string, delimiter, []]}}
  end

  def new(args) do
    {:ok, %__MODULE__{arguments: args}}
  end

  def evaluate(%{arguments: [value, delimiter, opts]}) do
    split(value, delimiter, opts)
  end

  defp split(value, delimiter, opts) do
    split_opts =
      if opts[:trim?] do
        [trim: true]
      else
        []
      end

    {:known, String.split(value, delimiter, split_opts)}
  end
end
