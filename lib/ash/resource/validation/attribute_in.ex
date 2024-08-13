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
  import Ash.Expr

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
  def atomic(_changeset, opts, context) do
    {:atomic, [opts[:attribute]], expr(^atomic_ref(opts[:attribute]) not in ^opts[:list]),
     expr(
       error(^InvalidAttribute, %{
         field: ^opts[:attribute],
         value: ^atomic_ref(opts[:attribute]),
         message: ^(context.message || "must be in %{list}"),
         vars: %{field: ^opts[:attribute], list: ^opts[:list]}
       })
     )}
  end

  @impl true
  def describe(opts) do
    [
      message: "must be in %{list}",
      vars: [field: opts[:attribute], list: opts[:list]]
    ]
  end
end
