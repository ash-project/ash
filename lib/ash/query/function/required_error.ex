defmodule Ash.Query.Function.RequiredError do
  @moduledoc """
  Returns the given value when it is present, or a required error when it is nil.

  Intended for required-attribute validation. It is usually used via an expression
  like `expr(ash_required!(^value, ^attribute))`, where `attribute` is a map or
  struct that at least contains `:name` and `:resource`.
  """

  use Ash.Query.Function, name: :ash_required!, predicate?: false

  @impl true
  def args, do: [[:any, :any]]

  @impl true
  def new([value_expr, attribute]) when is_struct(attribute) or is_map(attribute) do
    {:ok, %__MODULE__{arguments: [value_expr, attribute]}}
  end

  def new(_), do: {:error, "ash_required! expects (value, attribute)"}

  @impl true
  def evaluate(%{arguments: [value, attribute]}) do
    if is_nil(value) do
      resource =
        Map.get(attribute, :resource) ||
          raise("attribute must have :resource for ash_required!")

      field =
        Map.get(attribute, :name) ||
          Map.get(attribute, "name") ||
          raise("attribute must have :name for ash_required!")

      {:error,
       Ash.Error.Changes.Required.exception(
         field: field,
         type: :attribute,
         resource: resource
       )}
    else
      {:known, value}
    end
  end

  @impl true
  def can_return_nil?(_), do: false

  @impl true
  def evaluate_nil_inputs?, do: true

  @impl true
  def returns, do: :unknown
end
