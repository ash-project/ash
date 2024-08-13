defmodule Ash.Resource.Validation.ArgumentDoesNotEqual do
  @moduledoc false

  use Ash.Resource.Validation

  alias Ash.Error.Changes.InvalidArgument

  @opt_schema [
    argument: [
      type: :atom,
      required: true,
      doc: "The argument to check"
    ],
    value: [
      type: :any,
      required: true,
      doc: "The value the argument must not equal"
    ]
  ]

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

    if value == opts[:value] do
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
      message: "must not equal %{value}",
      vars: [field: opts[:argument], value: opts[:value]]
    ]
  end
end
