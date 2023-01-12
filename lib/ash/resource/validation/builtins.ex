defmodule Ash.Resource.Validation.Builtins do
  @moduledoc """
  Built in validations that are available to all resources

  The functions in this module are imported by default in the validations section.
  """

  alias Ash.Resource.Validation

  @doc """
  Validates that an attribute's value is in a given list

  ## Examples

      validate one_of(:status, [:closed_won, :closed_lost])
  """
  @spec one_of(attribute :: atom, list(any)) :: Validation.ref()
  def one_of(attribute, values) do
    {Validation.OneOf, attribute: attribute, values: values}
  end

  @doc """
  Validates that an attribute or relationship is being changed

  ## Examples

      validate changing(:first_name)
      validate changing(:comments)
  """
  @spec changing(attribute :: atom) :: Validation.ref()
  def changing(field) do
    {Validation.Changing, field: field}
  end

  @doc """
  Validates that a field or argument matches another field or argument

  ## Examples

      validate confirm(:password, :password_confirmation)
      validate confirm(:email, :email_confirmation)
  """
  @spec confirm(attribute_or_argument :: atom, confirmation_attribute_or_argument :: atom) ::
          Validation.ref()
  def confirm(field, confirmation) do
    {Validation.Confirm, [field: field, confirmation: confirmation]}
  end

  @doc """
  Validates that an attribute is not being changed to a specific value, or does not equal the given value if it is not being changed.

  ## Examples

      validate attribute_does_not_equal(:admin, true)

      # Or to only check for changing to a given value
      validate attribute_does_not_equal(:admin, true), where: [changing(:admin)]
  """
  @spec attribute_does_not_equal(attribute :: atom, value :: term) :: Validation.ref()
  def attribute_does_not_equal(attribute, value) do
    {Validation.AttributeDoesNotEqual, attribute: attribute, value: value}
  end

  @doc """
  Validates that the action is a specific action. Primarily meant for use in `where`.

  ## Examples

      validate present(:foo), where: [action_is(:bar)]
  """
  @spec action_is(action :: atom) :: Validation.ref()
  def action_is(action) do
    {Validation.ActionIs, action: action}
  end

  @doc """
  Validates that an attribute is being changed to a specific value, or equals the given value if it is not being changed.

  ## Examples

      validate attribute_equals(:admin, true)

      # Or to only check for changing to a given value
      validate attribute_equals(:admin, true), where: [changing(:admin)]
  """
  @spec attribute_equals(attribute :: atom, value :: term) :: Validation.ref()
  def attribute_equals(attribute, value) do
    {Validation.AttributeEquals, attribute: attribute, value: value}
  end

  @doc """
  Validates that an attribute on the original record meets the given length criteria

  ## Options

  #{Spark.OptionsHelpers.docs(Keyword.delete(Ash.Resource.Validation.StringLength.opt_schema(), :attribute))}

  ## Examples

      validate string_length(:slug, exactly: 8)
      validate string_length(:password, min: 6)
      validate string_length(:secret, min: 4, max: 12)
  """
  @spec string_length(attribute :: atom, opts :: Keyword.t()) :: Validation.ref()
  def string_length(attribute, opts \\ []) do
    {Validation.StringLength, Keyword.merge(opts, attribute: attribute)}
  end

  @numericality_docs """
  Validates that an attribute or argument meets the given comparison criteria.

  The values provided for each option may be a literal value, attribute, argument, or a zero argument function.

  ## Options
  #{Spark.OptionsHelpers.docs(Keyword.delete(Ash.Resource.Validation.Compare.opt_schema(), :attribute))}

  ## Examples

      validate numericality(:age, greater_than_or_equal_to: 18),
        where: [attribute_equals(:show_adult_content, true)],
        message: "Must be over %{greater_than_or_equal_to} to enable adult content."

      validate numericality(:points, greater_than: 0, less_than_or_equal_to: 100)
  """
  @doc @numericality_docs
  @spec numericality(attribute :: atom, opts :: Keyword.t()) :: Validation.ref()
  def numericality(attribute, opts \\ []) do
    compare(attribute, opts)
  end

  @doc String.replace(@numericality_docs, "numericality(", "compare(")
  @spec compare(attribute :: atom, opts :: Keyword.t()) :: Validation.ref()
  def compare(attribute, opts \\ []) do
    {Validation.Compare, Keyword.merge(opts, attribute: attribute)}
  end

  @doc """
  Validates that an attribute's value matches a given regex.

  `String.match?/2` is used to determine if the value matches.

  ## Examples

    validate match(:slug, ~r/^[0-9a-z-_]+$/)
  """
  @spec match(attribute :: atom, match :: Regex.t()) :: Validation.ref()
  def match(attribute, match) do
    {Validation.Match,
     attribute: attribute, match: match, message: "must match #{inspect(match)}"}
  end

  @doc """
  Validates the presence of a list of attributes or arguments.

  If no options are provided, validates that they are all present.

  ## Options

  #{Spark.OptionsHelpers.docs(Keyword.delete(Validation.Present.schema(), :attributes))}
  """
  @spec present(attributes_or_arguments :: atom | list(atom), opts :: Keyword.t()) ::
          Validation.ref()
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
  Validates the absence of a list of attributes or arguments.

  If no options are provided, validates that they are all absent.

  This works by changing your options and providing them to the `present` validation.

  ## Options

  #{String.replace(Spark.OptionsHelpers.docs(Keyword.delete(Validation.Present.schema(), :attributes)), "present", "absent")}
  """
  @spec absent(attributes_or_arguments :: atom | list(atom), opts :: Keyword.t()) ::
          Validation.ref()
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
