# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Validation.ByteSize do
  @moduledoc false
  use Ash.Resource.Validation
  import Ash.Gettext

  alias Ash.Error.Changes.InvalidAttribute
  import Ash.Expr

  @opt_schema [
    min: [
      type: :non_neg_integer,
      doc: "String must be this byte size at least"
    ],
    max: [
      type: :non_neg_integer,
      doc: "String must be this byte size at most"
    ],
    exact: [
      type: :non_neg_integer,
      doc: "String must be this byte size exactly"
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
  end

  @impl true
  def atomic(changeset, opts, context) do
    case Ash.Changeset.fetch_argument(changeset, opts[:attribute]) do
      {:ok, _} ->
        validate(changeset, opts, context)

      :error ->
        error_value =
          if Ash.Resource.Validation.should_redact?(changeset, opts[:attribute]) do
            Ash.Helpers.redact(nil)
          else
            atomic_ref(opts[:attribute])
          end

        opts
        |> Keyword.delete(:attribute)
        |> Enum.map(fn
          {:min, min} ->
            {:atomic, [opts[:attribute]], expr(byte_size(^atomic_ref(opts[:attribute])) < ^min),
             expr(
               error(
                 Ash.Error.Changes.InvalidAttribute,
                 %{
                   field: ^opts[:attribute],
                   value: ^error_value,
                   message:
                     ^(context.message || error_message("must have byte size of at least %{min}")),
                   vars: %{min: ^min}
                 }
               )
             )}

          {:max, max} ->
            {:atomic, [opts[:attribute]], expr(byte_size(^atomic_ref(opts[:attribute])) > ^max),
             expr(
               error(
                 Ash.Error.Changes.InvalidAttribute,
                 %{
                   field: ^opts[:attribute],
                   value: ^error_value,
                   message:
                     ^(context.message || error_message("must have byte size of at most %{max}")),
                   vars: %{max: ^max}
                 }
               )
             )}

          {:exact, exact} ->
            {:atomic, [opts[:attribute]],
             expr(byte_size(^atomic_ref(opts[:attribute])) != ^exact),
             expr(
               error(
                 Ash.Error.Changes.InvalidAttribute,
                 %{
                   field: ^opts[:attribute],
                   value: ^error_value,
                   message:
                     ^(context.message || error_message("must have byte size of exactly %{exact}")),
                   vars: %{exact: ^exact}
                 }
               )
             )}
        end)
    end
  end

  defp do_validate(subject, value, %{exact: exact} = opts) do
    if byte_size(value) == exact do
      :ok
    else
      {:error, exception(subject, value, opts)}
    end
  end

  defp do_validate(subject, value, %{min: min, max: max} = opts) do
    byte_size = byte_size(value)

    if byte_size >= min and byte_size <= max do
      :ok
    else
      {:error, exception(subject, value, opts)}
    end
  end

  defp do_validate(subject, value, %{min: min} = opts) do
    if byte_size(value) >= min do
      :ok
    else
      {:error, exception(subject, value, opts)}
    end
  end

  defp do_validate(subject, value, %{max: max} = opts) do
    if byte_size(value) <= max do
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
    do: [message: error_message("must have byte size of exactly %{exact}"), vars: [exact: exact]]

  def describe(%{min: min, max: max}),
    do: [
      message: error_message("must have byte size of between %{min} and %{max}"),
      vars: [min: min, max: max]
    ]

  def describe(%{min: min}),
    do: [message: error_message("must have byte size of at least %{min}"), vars: [min: min]]

  def describe(%{max: max}),
    do: [message: error_message("must have byte size of no more than %{max}"), vars: [max: max]]

  def describe(_opts), do: [message: inspect(__MODULE__), vars: []]
end
