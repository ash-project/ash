# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Validation.StringLength do
  @moduledoc false
  use Ash.Resource.Validation
  import Ash.Gettext

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
    count: [
      type: {:one_of, [:graphemes, :codepoints, :bytes]},
      doc: """
      The unit to count length in. `:graphemes` matches `String.length/1`, `:codepoints` matches
      how most SQL data layers count string length, and `:bytes` matches `byte_size/1`.
      Defaults to the unit implied by `config :ash, :default_string_length_count`
      (`:codepoints`, or `:graphemes` for `:mixed`).

      A single grapheme may contain an unbounded number of codepoints, so prefer `:codepoints` or
      `:bytes` when `max` is used as a storage or safety limit.

      Data layers cannot count graphemes, so with an explicit `:graphemes` this validation is not
      atomic when the attribute is being changed with an expression.
      """
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
  def supports(_opts), do: [Ash.Changeset, Ash.Query, Ash.ActionInput]

  @impl true
  def validate(subject, opts, _context) do
    case Ash.Subject.get_argument_or_attribute(subject, opts[:attribute]) do
      nil ->
        :ok

      value ->
        validate_value(subject, value, opts)
    end
  end

  defp validate_value(_subject, nil, _opts), do: :ok

  defp validate_value(subject, value, opts) do
    result =
      try do
        {:ok, to_string(value)}
      rescue
        _ ->
          {:error,
           InvalidAttribute.exception(
             value: Ash.Resource.Validation.maybe_redact(subject, opts[:attribute], value),
             field: opts[:attribute],
             message: error_message("could not be parsed")
           )}
      end

    case result do
      {:ok, str_value} ->
        do_validate(subject, str_value, Enum.into(opts, %{}))

      {:error, error} ->
        {:error, error}
    end
  end

  @impl true
  def atomic(changeset, opts, context) do
    # `nil` means neither the option nor the application config chose a unit, in which
    # case we keep the legacy behavior of `string_length/1`, whatever the data layer counts.
    count = Ash.Type.String.explicit_length_count(length_count: opts[:count])

    with :error <- Ash.Changeset.fetch_argument(changeset, opts[:attribute]),
         {:graphemes, {:ok, value}} <- {count, Keyword.fetch(changeset.atomics, opts[:attribute])} do
      # Data layers cannot count graphemes, so we can only validate literal values
      if Ash.Expr.expr?(value) do
        {:not_atomic,
         "can't atomically run string length validation counting graphemes on attribute `#{opts[:attribute]}` that is being atomically changed. Use `count: :codepoints` or `count: :bytes`."}
      else
        validate_value(changeset, value, opts)
      end
    else
      {:ok, _argument} ->
        validate(changeset, opts, context)

      {:graphemes, :error} ->
        # Not being changed atomically, so validate the literal (or current) value
        validate(changeset, opts, context)

      {_count, _} ->
        atomic_expression(changeset, opts, context, count)
    end
  end

  defp atomic_expression(changeset, opts, context, count) do
    length =
      case count do
        nil -> expr(string_length(^atomic_ref(opts[:attribute])))
        count -> expr(string_length(^atomic_ref(opts[:attribute]), ^count))
      end

    error_value =
      if Ash.Resource.Validation.should_redact?(changeset, opts[:attribute]) do
        Ash.Helpers.redact(nil)
      else
        atomic_ref(opts[:attribute])
      end

    opts
    |> Keyword.drop([:attribute, :count])
    |> Enum.map(fn
      {:min, min} ->
        {:atomic, [opts[:attribute]], expr(^length < ^min),
         expr(
           error(
             Ash.Error.Changes.InvalidAttribute,
             %{
               field: ^opts[:attribute],
               value: ^error_value,
               message:
                 ^(context.message || error_message("must have length of at least %{min}")),
               vars: %{min: ^min}
             }
           )
         )}

      {:max, max} ->
        {:atomic, [opts[:attribute]], expr(^length > ^max),
         expr(
           error(
             Ash.Error.Changes.InvalidAttribute,
             %{
               field: ^opts[:attribute],
               value: ^error_value,
               message: ^(context.message || error_message("must have length of at most %{max}")),
               vars: %{max: ^max}
             }
           )
         )}

      {:exact, exact} ->
        {:atomic, [opts[:attribute]], expr(^length != ^exact),
         expr(
           error(
             Ash.Error.Changes.InvalidAttribute,
             %{
               field: ^opts[:attribute],
               value: ^error_value,
               message:
                 ^(context.message || error_message("must have length of exactly %{exact}")),
               vars: %{exact: ^exact}
             }
           )
         )}
    end)
  end

  defp do_validate(subject, value, %{exact: exact} = opts) do
    if string_length(value, opts) == exact do
      :ok
    else
      {:error, exception(subject, value, opts)}
    end
  end

  defp do_validate(subject, value, %{min: min, max: max} = opts) do
    string_length = string_length(value, opts)

    if string_length >= min and string_length <= max do
      :ok
    else
      {:error, exception(subject, value, opts)}
    end
  end

  defp do_validate(subject, value, %{min: min} = opts) do
    if string_length(value, opts) >= min do
      :ok
    else
      {:error, exception(subject, value, opts)}
    end
  end

  defp do_validate(subject, value, %{max: max} = opts) do
    if string_length(value, opts) <= max do
      :ok
    else
      {:error, exception(subject, value, opts)}
    end
  end

  defp string_length(value, opts) do
    count = Map.get(opts, :count) || Ash.Type.String.default_length_count()
    Ash.Query.Function.StringLength.string_length(value, count)
  end

  defp exception(subject, value, opts) do
    [
      value: Ash.Resource.Validation.maybe_redact(subject, opts[:attribute], value),
      field: opts[:attribute]
    ]
    |> with_description(opts)
    |> InvalidAttribute.exception()
  end

  @impl true
  def describe(%{exact: exact}),
    do: [message: error_message("must have length of exactly %{exact}"), vars: [exact: exact]]

  def describe(%{min: min, max: max}),
    do: [
      message: error_message("must have length of between %{min} and %{max}"),
      vars: [min: min, max: max]
    ]

  def describe(%{min: min}),
    do: [message: error_message("must have length of at least %{min}"), vars: [min: min]]

  def describe(%{max: max}),
    do: [message: error_message("must have length of no more than %{max}"), vars: [max: max]]

  def describe(_opts), do: [message: inspect(__MODULE__), vars: []]
end
