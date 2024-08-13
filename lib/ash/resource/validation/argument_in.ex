defmodule Ash.Resource.Validation.ArgumentIn do
  @moduledoc false

  @opt_schema [
    argument: [
      type: :atom,
      required: true,
      doc: "The argument to check"
    ],
    list: [
      type: :any,
      required: true,
      doc: "The list of values that the argument must be in"
    ]
  ]

  use Ash.Resource.Validation
  alias Ash.Error.Changes.InvalidArgument

  opt_schema = @opt_schema

  defmodule Opts do
    @moduledoc false
    use Spark.Options.Validator, schema: opt_schema
  end

  @impl true
  def init(opts) do
    case Opts.validate(opts) do
      {:ok, opts} ->
        {:ok, Opts.to_options(opts)}

      {:error, error} ->
        {:error, Exception.message(error)}
    end
  end

  @impl true
  def validate(changeset, opts, _context) do
    value = Ash.Changeset.get_argument(changeset, opts[:argument])

    if value in opts[:list] do
      :ok
    else
      {:error,
       [value: value, field: opts[:argument]]
       |> with_description(opts)
       |> InvalidArgument.exception()}
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
