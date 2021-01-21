defmodule Ash.Resource.Validation.Match do
  @moduledoc false
  alias Ash.Error.Changes.InvalidAttribute

  use Ash.Resource.Validation

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
    case Ash.OptionsHelpers.validate(opts, @opt_schema) do
      {:ok, opts} ->
        {:ok, opts}

      {:error, error} ->
        {:error, Exception.message(error)}
    end
  end

  @impl true
  def validate(changeset, opts) do
    case Ash.Changeset.fetch_change(changeset, opts[:attribute]) do
      {:ok, changing_to} when is_binary(changing_to) ->
        if String.match?(changing_to, opts[:match]) do
          :ok
        else
          {:error,
           InvalidAttribute.exception(
             field: opts[:attribute],
             message: opts[:message],
             vars: [match: opts[:match]]
           )}
        end

      _ ->
        :ok
    end
  end
end
