defmodule Ash.Resource.Validation.AttributeDoesNotEqual do
  @moduledoc "A validation that fails unless the attribute does not equal a specific value"
  use Ash.Resource.Validation

  alias Ash.Error.Changes.InvalidAttribute

  @opt_schema [
    attribute: [
      type: :atom,
      required: true,
      doc: "The attribute to check"
    ],
    value: [
      type: :any,
      required: true,
      doc: "The value the attribute must not equal"
    ]
  ]

  @impl true
  def init(opts) do
    case Ash.OptionsHelpers.validate(opts, @opt_schema) do
      {:ok, opts} ->
        {:ok, opts}

      {:error, error} ->
        {:error, Exception.message(error)}
    end
  end

  @impl true
  def validate(changeset, opts) do
    if changeset.data && Map.get(changeset.data, opts[:attribute]) == opts[:value] do
      {:error,
       InvalidAttribute.exception(
         field: opts[:attribute],
         message: "must not equal %{value}",
         vars: [field: opts[:attribute], value: opts[:value]]
       )}
    else
      :ok
    end
  end
end
