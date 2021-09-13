defmodule Ash.Resource.Validation.AttributeEquals do
  @moduledoc """
  A validation that fails unless the attribute equals a specific value *before* changes are applied.

  Creates, however, will take into account the changes.
  """
  @opt_schema [
    attribute: [
      type: :atom,
      required: true,
      doc: "The attribute to check"
    ],
    value: [
      type: :any,
      required: true,
      doc: "The value the attribute must equal"
    ]
  ]

  use Ash.Resource.Validation
  alias Ash.Error.Changes.InvalidAttribute

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
    cond do
      changeset.action_type == :create &&
          Ash.Changeset.get_attribute(changeset, opts[:attribute]) != opts[:value] ->
        {:error,
         InvalidAttribute.exception(
           field: opts[:attribute],
           message: "must equal %{value}",
           vars: [field: opts[:attribute], value: opts[:value]]
         )}

      changeset.data.action_type != :create && changeset.data &&
          Map.get(changeset.data, opts[:attribute]) != opts[:value] ->
        {:error,
         InvalidAttribute.exception(
           field: opts[:attribute],
           message: "must equal %{value}",
           vars: [field: opts[:attribute], value: opts[:value]]
         )}

      true ->
        :ok
    end
  end
end
