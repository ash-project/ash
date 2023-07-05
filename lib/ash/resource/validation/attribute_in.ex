defmodule Ash.Resource.Validation.AttributeIn do
  @moduledoc false

  @opt_schema [
    attribute: [
      type: :atom,
      required: true,
      doc: "The attribute to check"
    ],
    list: [
      type: :any,
      required: true,
      doc: "The list of values that the attribute must be in"
    ]
  ]

  use Ash.Resource.Validation
  alias Ash.Error.Changes.InvalidAttribute

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
    value = Ash.Changeset.get_attribute(changeset, opts[:attribute])

    if value in opts[:list] do
      :ok
    else
      {:error,
       [value: value, field: opts[:attribute]]
       |> with_description(opts)
       |> InvalidAttribute.exception()}
    end
  end

  @impl true
  def describe(opts) do
    [
      message: "must equal %{value}",
      vars: [field: opts[:attribute], value: opts[:value]]
    ]
  end
end
