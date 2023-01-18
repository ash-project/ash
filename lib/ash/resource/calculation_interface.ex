defmodule Ash.Resource.CalculationInterface do
  @moduledoc """
  Represents a function that evaluates a calculation in a resource's code interface
  """
  defstruct [:name, :calculation, :args]

  @type t :: %__MODULE__{}

  @schema [
    name: [
      type: :atom,
      doc: "The name of the function that will be defined",
      required: true
    ],
    calculation: [
      type: :atom,
      doc:
        "The name of the calculation that will be evaluated. Defaults to the same name as the function."
    ],
    args: [
      type: :any,
      default: [],
      doc: """
      Supply field or argument values referenced by the calculation.

      By default, the value will be provided for any matching named reference *and* argument.
      This is normally fine, but in the case that you have an argument and a reference with the same name,
      you can specify it by supplying `{:arg, :name}` and `{:ref, :name}`. For example:

      ```elixir
      define_calculation :id_matches, args: [{:arg, :id}, {:ref, :id}]
      ```

      To make arguments optional, wrap them in `{:optional, ..}`, for example:

      ```elixir
      define_calculation :id_matches, args: [{:arg, :id}, {:optional, {:ref, :id}}]
      ```
      """
    ]
  ]

  def schema, do: @schema

  def transform(%{name: name, calculation: nil} = interface) do
    {:ok, %{interface | calculation: name}}
  end

  def transform(interface), do: {:ok, interface}
end
