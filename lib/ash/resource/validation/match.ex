defmodule Ash.Resource.Validation.Match do
  @moduledoc false

  use Ash.Resource.Validation

  alias Ash.Error.Changes.InvalidAttribute

  @opt_schema [
    attribute: [
      type: :atom,
      required: true,
      doc: "The attribute to check"
    ],
    match: [
      type: :any,
      required: true,
      doc: "The value that the attribute should match against"
    ],
    message: [
      type: :string,
      required: true,
      doc: "The message that will be placed on the field in the case of failure"
    ]
  ]

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
    case Ash.Changeset.fetch_argument_or_change(changeset, opts[:attribute]) do
      {:ok, changing_to} when not is_nil(changing_to) ->
        case string_value(changing_to, opts) do
          {:ok, changing_to} ->
            if String.match?(changing_to, opts[:match]) do
              :ok
            else
              {:error,
               InvalidAttribute.exception(
                 field: opts[:attribute],
                 value: changing_to,
                 message: opts[:message],
                 vars: [match: opts[:match]]
               )}
            end

          {:error, error} ->
            {:error, error}
        end

      _ ->
        :ok
    end
  end

  defp string_value(value, opts) do
    {:ok, to_string(value)}
  rescue
    _ ->
      {:error,
       InvalidAttribute.exception(
         value: opts[:value],
         field: opts[:attribute],
         message: opts[:message],
         vars: [match: opts[:match]]
       )}
  end
end
