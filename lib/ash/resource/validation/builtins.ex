defmodule Ash.Resource.Validation.Builtins do
  @moduledoc """
  Built in validations that are available to all resources

  The functions in this module are imported by default in the validations section.
  """

  alias Ash.Resource.Validation

  @doc """
  Validates that an attribute's value is in a given list
  """
  def one_of(attribute, values) do
    {Validation.OneOf, attribute: attribute, values: values}
  end

  @doc """
  Validates the presence of a list of attributes

  If no options are provided, validates that they are all present.

  #{Ash.OptionsHelpers.docs(Keyword.delete(Validation.Present.schema(), :attributes))}
  """
  def present(attributes, opts \\ []) do
    if opts == [] do
      attributes = List.wrap(attributes)
      {Validation.Present, attributes: attributes, exactly: Enum.count(attributes)}
    else
      opts = Keyword.put(opts, :attributes, List.wrap(attributes))
      {Validation.Present, opts}
    end
  end

  @doc """
  Validates the absence of a list of attributes

  If no options are provided, validates that they are all absent.

  The docs behave the same as `present/2`, except they validate absence.
  """
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
