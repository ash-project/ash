defmodule Ash.Resource.Validation.Negate do
  @moduledoc false

  @opt_schema [
    validation: [
      type: Ash.Resource.Validation.validation_type(),
      required: true,
      doc: "The validation module to negate another validation"
    ]
  ]

  use Ash.Resource.Validation
  alias Ash.Error.Changes.InvalidAttribute
  alias Ash.Error.Changes.InvalidChanges
  require Ash.Expr

  @impl true
  def init(opts) do
    with {:ok, opts} <- Spark.Options.validate(opts, @opt_schema),
         {validation, validation_opts} = opts[:validation],
         {:module, validation} <- Code.ensure_compiled(validation),
         true <- function_exported?(validation, :describe, 1),
         {:ok, validation_opts} <- validation.init(validation_opts) do
      opts = Keyword.put(opts, :validation, {validation, validation_opts})
      {:ok, opts}
    else
      false ->
        module =
          case opts[:validation] do
            {module, _opts} -> module
            module -> module
          end

        raise ArgumentError,
              "#{inspect(module)} must implement `describe/1` function to be used in #{__MODULE__}"

      error ->
        error
    end
  end

  @impl true
  def validate(changeset, opts, context) do
    {validation, validation_opts} = opts[:validation]

    case validation.validate(changeset, validation_opts, context) do
      {:error, _} ->
        :ok

      :ok ->
        {:error,
         opts
         |> describe()
         |> InvalidAttribute.exception()}
    end
  end

  @impl true
  def atomic(changeset, opts, context) do
    {validation, validation_opts} = opts[:validation]

    {message, vars} =
      if context.message do
        {context.message, []}
      else
        case validation.describe(validation_opts) do
          message when is_binary(message) ->
            {message, []}

          options ->
            {"must not pass validation: #{options[:message]}", options[:vars] || []}
        end
      end

    case validation.atomic(changeset, validation_opts, context) do
      list when is_list(list) ->
        Enum.map(list, &negate_atomic(&1, message, vars))

      {:atomic, _fields, _condition, _error_expr} = atomic ->
        negate_atomic(atomic, message, vars)

      {:error, _} ->
        :ok

      :ok ->
        {:error,
         opts
         |> describe()
         |> InvalidAttribute.exception()}

      {:not_atomic, reason} ->
        {:not_atomic, reason}
    end
  end

  defp negate_atomic({:atomic, fields, condition, _error_expr}, message, vars) do
    {:atomic, fields, Ash.Expr.expr(not (^condition)),
     Ash.Expr.expr(
       error(^InvalidChanges, %{
         fields: fields,
         message: ^message,
         vars: ^Map.new(vars)
       })
     )}
  end

  @impl true
  def describe(opts) do
    {validation, validation_opts} = opts[:validation]

    case validation.describe(validation_opts) do
      message when is_binary(message) ->
        [message: message, vars: []]

      options ->
        [message: "must not pass validation: #{options[:message]}", vars: options[:vars] || []]
    end
  end
end
