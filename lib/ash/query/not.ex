defmodule Ash.Query.Not do
  @moduledoc "Represents the negation of the contained expression"
  defstruct [:expression]

  def new(nil), do: nil

  def new(%__MODULE__{expression: expression}), do: expression

  def new(true), do: false
  def new(false), do: true

  def new(expression) do
    %__MODULE__{expression: expression}
  end

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(%{expression: expression}, opts) do
      opts = put_container_type(opts)

      concat(["not ", to_doc(expression, opts)])
    end

    # custom options not available before Elixir 1.9

    defp put_container_type(opts) do
      custom_options = apply(Map, :get, [opts, :custom_options])

      apply(Map, :put, [
        opts,
        :custom_options,
        Keyword.put(custom_options, :container_type, :not)
      ])

      # above version required to avoid dialyzer warnings on lack of custom_options in pre-1.9 elixir
      # %{opts | custom_options: Keyword.put(opts.custom_options, :container_type, container_type)}
    end
  end
end
