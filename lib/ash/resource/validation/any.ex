# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Validation.Any do
  @moduledoc false

  @opt_schema [
    validations: [
      type: {:list, Ash.Resource.Validation.validation_type()},
      required: true,
      doc: "The list of validations, at least one of which must pass"
    ]
  ]

  use Ash.Resource.Validation
  alias Ash.Error.Changes.InvalidAttribute
  alias Ash.Error.Changes.InvalidChanges
  require Ash.Expr

  opt_schema = @opt_schema

  defmodule Opts do
    @moduledoc false
    use Spark.Options.Validator, schema: opt_schema
  end

  @impl true
  def init(opts) do
    with {:ok, opts} <- Opts.validate(opts),
         opts <- Opts.to_options(opts),
         validations <- opts[:validations] do
      validated_validations =
        Enum.map(validations, fn validation_spec ->
          {validation, validation_opts} =
            case validation_spec do
              {validation, validation_opts} -> {validation, validation_opts}
              validation -> {validation, []}
            end

          case Code.ensure_compiled(validation) do
            {:module, ^validation} ->
              if function_exported?(validation, :describe, 1) do
                {:ok, validation_opts} = validation.init(validation_opts)
                {validation, validation_opts}
              else
                raise ArgumentError,
                      "#{inspect(validation)} must implement `describe/1` function to be used in #{__MODULE__}"
              end

            _ ->
              raise ArgumentError, "#{inspect(validation)} is not a valid module"
          end
        end)

      opts = Keyword.put(opts, :validations, validated_validations)
      {:ok, opts}
    else
      error ->
        error
    end
  end

  @impl true
  def supports(opts) do
    validations = opts[:validations]

    case validations do
      [] ->
        []

      [{first_validation, first_opts} | rest] ->
        first_supports = first_validation.supports(first_opts)

        Enum.reduce(rest, first_supports, fn {validation, validation_opts}, acc ->
          validation_supports = validation.supports(validation_opts)
          Enum.filter(acc, &(&1 in validation_supports))
        end)
    end
  end

  @impl true
  def validate(subject, opts, context) do
    validations = opts[:validations]

    results =
      Enum.map(validations, fn {validation, validation_opts} ->
        validation.validate(subject, validation_opts, context)
      end)

    case Enum.any?(results, &(&1 == :ok)) do
      true ->
        :ok

      false ->
        {:error,
         opts
         |> describe()
         |> InvalidAttribute.exception()}
    end
  end

  @impl true
  def atomic(changeset, opts, context) do
    validations = opts[:validations]

    {message, vars} =
      if context.message do
        {context.message, []}
      else
        desc = describe(opts)
        {desc[:message], desc[:vars] || []}
      end

    atomic_results =
      Enum.map(validations, fn {validation, validation_opts} ->
        validation.atomic(changeset, validation_opts, context)
      end)
      |> List.flatten()

    case Enum.find(atomic_results, &short_circuits?/1) do
      nil ->
        atomic_results
        |> Enum.reject(&match?({:error, _}, &1))
        |> Enum.reduce({[], true}, fn
          {:atomic, new_fields, new_condition, _error_expr}, {fields, condition} ->
            {Enum.uniq(fields ++ new_fields), Ash.Expr.expr(^new_condition and ^condition)}
        end)
        |> case do
          {fields, true} ->
            {:error, InvalidChanges.exception(fields: fields, message: message, vars: vars)}

          {fields, condition} ->
            {:atomic, fields, condition,
             expr(
               error(^InvalidChanges, %{fields: ^fields, message: ^message, vars: ^Map.new(vars)})
             )}
        end

      response ->
        response
    end
  end

  defp short_circuits?(:ok), do: true
  defp short_circuits?({:not_atomic, _reason}), do: true
  defp short_circuits?(_), do: false

  @impl true
  def describe(opts) do
    validations = opts[:validations]

    validation_messages =
      Enum.map(validations, fn {validation, validation_opts} ->
        case validation.describe(validation_opts) do
          message when is_binary(message) -> message
          options -> options[:message]
        end
      end)

    message = "must pass at least one of: #{Enum.join(validation_messages, ", ")}"
    [message: message, vars: []]
  end
end
