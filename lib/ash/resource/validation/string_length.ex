defmodule Ash.Resource.Validation.StringLength do
  @moduledoc false
  use Ash.Resource.Validation

  alias Ash.Error.Changes.InvalidAttribute

  @opt_schema [
    attribute: [
      type: :atom,
      required: true
    ],
    min: [
      type: :non_neg_integer,
      doc: "String must be this length at least"
    ],
    max: [
      type: :non_neg_integer,
      doc: "String must be this length at most"
    ],
    exact: [
      type: :non_neg_integer,
      doc: "String must be this length exactly"
    ]
  ]

  def opt_schema, do: @opt_schema

  @impl true
  def init(opts) do
    case Spark.OptionsHelpers.validate(opts, @opt_schema) do
      {:ok, opts} ->
        {:ok, opts}

      {:error, error} ->
        {:error, Exception.message(error)}
    end
  end

  @impl true
  def validate(changeset, opts) do
    case Ash.Changeset.get_argument_or_attribute(changeset, opts[:attribute]) do
      nil ->
        :ok

      value ->
        value =
          try do
            {:ok, to_string(value)}
          rescue
            _ ->
              {:error,
               InvalidAttribute.exception(
                 value: value,
                 field: opts[:attribute],
                 message: "%{field} could not be parsed",
                 vars: [field: opts[:attribute]]
               )}
          end

        case value do
          {:ok, value} ->
            do_validate(value, Enum.into(opts, %{}))

          {:error, error} ->
            {:error, error}
        end
    end
  end

  defp do_validate(value, %{exact: exact} = opts) do
    if String.length(value) == exact do
      :ok
    else
      {:error,
       InvalidAttribute.exception(
         value: value,
         field: opts[:attribute],
         message: "%{field} must have length of exactly %{exact}",
         vars: [field: opts[:attribute], exact: exact]
       )}
    end
  end

  defp do_validate(value, %{min: min, max: max} = opts) do
    string_length = String.length(value)

    if string_length >= min and string_length <= max do
      :ok
    else
      {:error,
       InvalidAttribute.exception(
         value: value,
         field: opts[:attribute],
         message: "%{field} must have length of between %{min} and %{max}",
         vars: [field: opts[:attribute], min: min, max: max]
       )}
    end
  end

  defp do_validate(value, %{min: min} = opts) do
    if String.length(value) >= min do
      :ok
    else
      {:error,
       InvalidAttribute.exception(
         value: value,
         field: opts[:attribute],
         message: "%{field} must have length of at least %{min}",
         vars: [field: opts[:attribute], min: min]
       )}
    end
  end

  defp do_validate(value, %{max: max} = opts) do
    if String.length(value) <= max do
      :ok
    else
      {:error,
       InvalidAttribute.exception(
         value: value,
         field: opts[:attribute],
         message: "%{field} must have length of no more than %{max}",
         vars: [field: opts[:attribute], max: max]
       )}
    end
  end
end
