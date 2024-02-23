defmodule Ash.Resource.Validation.ArgumentEquals do
  @moduledoc false

  @opt_schema [
    argument: [
      type: :atom,
      required: true,
      doc: "The argument to check"
    ],
    value: [
      type: :any,
      required: true,
      doc: "The value the argument must equal"
    ]
  ]

  use Ash.Resource.Validation
  alias Ash.Error.Changes.InvalidArgument

  @impl true
  def init(opts) do
    case Spark.Options.validate(opts, @opt_schema) do
      {:ok, opts} ->
        {:ok, opts}

      {:error, error} ->
        {:error, Exception.message(error)}
    end
  end

  @impl true
  def validate(changeset, opts, _context) do
    value = Ash.Changeset.get_argument(changeset, opts[:argument])

    if value != opts[:value] do
      {:error,
       [field: opts[:argument], value: value]
       |> with_description(opts)
       |> InvalidArgument.exception()}
    else
      :ok
    end
  end

  @impl true
  def atomic(changeset, opts, context) do
    validate(changeset, opts, context)
  end

  @impl true
  def describe(opts) do
    [
      message: "must equal %{value}",
      vars: [field: opts[:argument], value: opts[:value]]
    ]
  end
end
