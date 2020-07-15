defmodule Ash.Resource.Validation.Builtins do
  @moduledoc """
  Built in validations that are available to all resources

  The functions in this module are imported by default in the validations section.
  """

  alias Ash.Resource.Validation

  def present(attributes, opts \\ []) do
    if opts == [] do
      attributes = List.wrap(attributes)
      {Validation.Present, attributes: attributes, exactly: Enum.count(attributes)}
    else
      opts = Keyword.put(opts, :attributes, List.wrap(attributes))
      {Validation.Present, opts}
    end
  end

  def absent(attributes, opts \\ []) do
    if opts == [] do
      {Validation.Present, attributes: List.wrap(attributes), exactly: 0}
    else
      attributes = List.wrap(attributes)
      count = Enum.count(attributes)

      new_opts =
        case Keyword.fetch(opts, :at_least) do
          {:ok, value} ->
            Keyword.put(opts, :at_most, count - value)

          :error ->
            Keyword.put(opts, :at_most, 0)
        end

      new_opts =
        case Keyword.fetch(opts, :at_most) do
          {:ok, value} ->
            Keyword.put(new_opts, :at_least, count - value)

          :error ->
            Keyword.put(new_opts, :at_least, 0)
        end

      present(attributes, new_opts)
    end
  end
end
