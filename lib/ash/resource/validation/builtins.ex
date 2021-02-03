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

  @doc "Validates that an attribute is being changed"
  def changing(field) do
    {Validation.Changing, field: field}
  end

  @doc "Validates that a field or argument matches another field or argument"
  def confirm(field, confirmation) do
    {Validation.Confirm, [field: field, confirmation: confirmation]}
  end

  @doc "Validates that an attribute on the original record does not equal a specific value"
  def attribute_does_not_equal(attribute, value) do
    {Validation.AttributeDoesNotEqual, attribute: attribute, value: value}
  end

  @doc "Validates that an attribute on the original record equals a specific value"
  def attribute_equals(attribute, value) do
    {Validation.AttributeEquals, attribute: attribute, value: value}
  end

  @doc "Validates that an attribute on the original record meets the given length criteria"
  def string_length(attribute, opts \\ []) do
    {Validation.StringLength, Keyword.merge(opts, attribute: attribute)}
  end

  @doc """
  Validates that an attribute's value matches a given regex or string, using the provided error, message if not.

  `String.match?/2` is used to determine if it matches.
  """

  def match(attribute, match, message \\ nil) do
    message = message || "must match #{match}"

    {Validation.Match, attribute: attribute, match: match, message: message}
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
