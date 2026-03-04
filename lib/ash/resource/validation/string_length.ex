# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

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
  def supports(_opts), do: [Ash.Changeset, Ash.Query, Ash.ActionInput]

  @impl true
  def validate(subject, opts, _context) do
    case Ash.Subject.get_argument_or_attribute(subject, opts[:attribute]) do
      nil ->
        :ok

      value ->
        result =
          try do
            {:ok, to_string(value)}
          rescue
            _ ->
              {:error,
               InvalidAttribute.exception(
                 value: Ash.Resource.Validation.maybe_redact(subject, opts[:attribute], value),
                 field: opts[:attribute],
                 message: "could not be parsed"
               )}
          end

        case result do
          {:ok, str_value} ->
            do_validate(subject, str_value, Enum.into(opts, %{}))

          {:error, error} ->
            {:error, error}
        end
    end
  end

  @impl true
  def atomic(changeset, opts, context) do
    case Ash.Changeset.fetch_argument(changeset, opts[:attribute]) do
      {:ok, _} ->
        validate(changeset, opts, context)

      :error ->
        error_value =
          if Ash.Resource.Validation.sensitive?(changeset, opts[:attribute]) do
            Ash.Helpers.redact(nil)
          else
            atomic_ref(opts[:attribute])
          end

        opts
        |> Keyword.delete(:attribute)
        |> Enum.map(fn
          {:min, min} ->
            {:atomic, [opts[:attribute]],
             expr(string_length(^atomic_ref(opts[:attribute])) < ^min),
             expr(
               error(
                 Ash.Error.Changes.InvalidAttribute,
                 %{
                   field: ^opts[:attribute],
                   value: ^error_value,
                   message: ^(context.message || "must have length of at least %{min}"),
                   vars: %{min: ^min}
                 }
               )
             )}

          {:max, max} ->
            {:atomic, [opts[:attribute]],
             expr(string_length(^atomic_ref(opts[:attribute])) > ^max),
             expr(
               error(
                 Ash.Error.Changes.InvalidAttribute,
                 %{
                   field: ^opts[:attribute],
                   value: ^error_value,
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
                   value: ^error_value,
                   message: ^(context.message || "must have length of exactly %{exact}"),
                   vars: %{exact: ^exact}
                 }
               )
             )}
        end)
    end
  end

  defp do_validate(subject, value, %{exact: exact} = opts) do
    if String.length(value) == exact do
      :ok
    else
      {:error, exception(subject, value, opts)}
    end
  end

  defp do_validate(subject, value, %{min: min, max: max} = opts) do
    string_length = String.length(value)

    if string_length >= min and string_length <= max do
      :ok
    else
      {:error, exception(subject, value, opts)}
    end
  end

  defp do_validate(subject, value, %{min: min} = opts) do
    if String.length(value) >= min do
      :ok
    else
      {:error, exception(subject, value, opts)}
    end
  end

  defp do_validate(subject, value, %{max: max} = opts) do
    if String.length(value) <= max do
      :ok
    else
      {:error, exception(subject, value, opts)}
    end
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
    do: [message: "must have length of exactly %{exact}", vars: [exact: exact]]

  def describe(%{min: min, max: max}),
    do: [message: "must have length of between %{min} and %{max}", vars: [min: min, max: max]]

  def describe(%{min: min}),
    do: [message: "must have length of at least %{min}", vars: [min: min]]

  def describe(%{max: max}),
    do: [message: "must have length of no more than %{max}", vars: [max: max]]

  def describe(_opts), do: [message: inspect(__MODULE__), vars: []]
end
