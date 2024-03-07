defmodule Ash.Resource.Validation.OneOf do
  @moduledoc false

  use Ash.Resource.Validation

  alias Ash.Error.Changes.InvalidAttribute
  require Ash.Expr
  import Ash.Filter.TemplateHelpers

  @opt_schema [
    values: [
      type: {:list, :any},
      required: true
    ],
    attribute: [
      type: :atom,
      required: true
    ]
  ]

  @doc false
  def values(value) when is_list(value), do: {:ok, value}
  def values(_), do: {:error, "Expected a list of values"}

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
  def validate(changeset, opts, _context) do
    case Ash.Changeset.fetch_argument_or_change(changeset, opts[:attribute]) do
      {:ok, nil} ->
        :ok

      {:ok, changing_to} ->
        if Enum.any?(opts[:values], &Comp.equal?(&1, changing_to)) do
          :ok
        else
          {:error,
           [value: changing_to, field: opts[:attribute]]
           |> with_description(opts)
           |> InvalidAttribute.exception()}
        end

      _ ->
        :ok
    end
  end

  @impl true
  def atomic(changeset, opts, context) do
    value =
      case Ash.Changeset.fetch_argument_or_change(changeset, opts[:attribute]) do
        {:ok, value} ->
          value

        :error ->
          expr(^atomic_ref(opts[:attribute]))
      end

    {:atomic, [opts[:attribute]], Ash.Expr.expr(^value not in ^opts[:values]),
     Ash.Expr.expr(
       error(
         Ash.Error.Changes.InvalidAttribute,
         %{
           field: ^opts[:attribute],
           value: ^value,
           message: ^(context[:message] || "expected one of %{values}"),
           vars: %{values: Enum.map_join(opts[:values], ", ", &to_string/1)}
         }
       )
     )}
  end

  @impl true
  def describe(opts) do
    [
      message: "expected one of %{values}",
      vars: [values: Enum.map_join(opts[:values], ", ", &to_string/1)]
    ]
  end
end
