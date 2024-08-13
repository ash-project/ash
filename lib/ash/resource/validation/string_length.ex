defmodule Ash.Resource.Validation.StringLength do
  @moduledoc false
  use Ash.Resource.Validation

  alias Ash.Error.Changes.InvalidAttribute
  import Ash.Expr

  @opt_schema [
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
    ],
    attribute: [
      type: :atom,
      required: true,
      hide: true
    ]
  ]

  def opt_schema, do: @opt_schema

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
                 message: "could not be parsed"
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

  @impl true
  def atomic(_changeset, opts, context) do
    opts
    |> Keyword.delete(:attribute)
    |> Enum.map(fn
      {:min, min} ->
        {:atomic, [opts[:attribute]], expr(string_length(^atomic_ref(opts[:attribute])) < ^min),
         expr(
           error(
             Ash.Error.Changes.InvalidAttribute,
             %{
               field: ^opts[:attribute],
               value: ^atomic_ref(opts[:attribute]),
               message: ^(context.message || "must have length of at least %{min}"),
               vars: %{min: ^min}
             }
           )
         )}

      {:max, max} ->
        {:atomic, [opts[:attribute]], expr(string_length(^atomic_ref(opts[:attribute])) > ^max),
         expr(
           error(
             Ash.Error.Changes.InvalidAttribute,
             %{
               field: ^opts[:attribute],
               value: ^atomic_ref(opts[:attribute]),
               message: ^(context.message || "must have length of at most %{max}"),
               vars: %{max: ^max}
             }
           )
         )}

      {:exact, exact} ->
        {:atomic, [opts[:attribute]],
         expr(string_length(^atomic_ref(opts[:attribute])) != ^exact),
         expr(
           error(
             Ash.Error.Changes.InvalidAttribute,
             %{
               field: ^opts[:attribute],
               value: ^atomic_ref(opts[:attribute]),
               message: ^(context.message || "must have length of at most %{exact}"),
               vars: %{exact: ^exact}
             }
           )
         )}
    end)
  end

  defp do_validate(value, %{exact: exact} = opts) do
    if String.length(value) == exact do
      :ok
    else
      {:error, exception(value, opts)}
    end
  end

  defp do_validate(value, %{min: min, max: max} = opts) do
    string_length = String.length(value)

    if string_length >= min and string_length <= max do
      :ok
    else
      {:error, exception(value, opts)}
    end
  end

  defp do_validate(value, %{min: min} = opts) do
    if String.length(value) >= min do
      :ok
    else
      {:error, exception(value, opts)}
    end
  end

  defp do_validate(value, %{max: max} = opts) do
    if String.length(value) <= max do
      :ok
    else
      {:error, exception(value, opts)}
    end
  end

  defp exception(value, opts) do
    [value: value, field: opts[:attribute]]
    |> with_description(opts)
    |> InvalidAttribute.exception()
  end

  @impl true
  def describe(%{exact: exact}),
    do: [message: "must have length of exactly %{exact}", vars: [exact: exact]]

  def describe(%{min: min, max: max}),
    do: [message: "must have length of between %{min} and %{max}", vars: [min: min, max: max]]

  def describe(%{min: min}),
    do: [message: "must have length of at least %{min}", vars: [min: min]]

  def describe(%{max: max}),
    do: [message: "must have length of no more than %{max}", vars: [max: max]]

  def describe(_opts), do: [message: inspect(__MODULE__), vars: []]
end
