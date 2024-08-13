defmodule Ash.Resource.Validation.OneOf do
  @moduledoc false

  use Ash.Resource.Validation

  alias Ash.Error.Changes.InvalidAttribute
  import Ash.Expr

  @opt_schema [
    values: [
      type: {:wrap_list, :any},
      required: true
    ],
    attribute: [
      type: :atom,
      required: true,
      hide: true
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
    value =
      if Enum.any?(changeset.action.arguments, &(&1.name == opts[:attribute])) do
        Ash.Changeset.fetch_argument(changeset, opts[:attribute])
      else
        {:ok, Ash.Changeset.get_attribute(changeset, opts[:attribute])}
      end

    case value do
      {:ok, nil} ->
        :ok

      {:ok, value} ->
        if Enum.any?(opts[:values], &Comp.equal?(&1, value)) do
          :ok
        else
          {:error,
           [value: value, field: opts[:attribute]]
           |> with_description(opts)
           |> InvalidAttribute.exception()}
        end

      :error ->
        :ok
    end
  end

  @impl true
  def atomic(changeset, opts, context) do
    if Enum.any?(changeset.action.arguments, &(&1.name == opts[:attribute])) do
      validate(changeset, opts, context)
    else
      value = expr(^atomic_ref(opts[:attribute]))

      {:atomic, [opts[:attribute]], expr(^value not in ^opts[:values]),
       expr(
         error(
           Ash.Error.Changes.InvalidAttribute,
           %{
             field: ^opts[:attribute],
             value: ^value,
             message: ^(context.message || "expected one of %{values}"),
             vars: %{values: ^Enum.map_join(opts[:values], ", ", &to_string/1)}
           }
         )
       )}
    end
  end

  @impl true
  def describe(opts) do
    [
      message: "expected one of %{values}",
      vars: [values: Enum.map_join(opts[:values], ", ", &to_string/1)]
    ]
  end
end
