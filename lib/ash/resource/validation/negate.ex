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

  @impl true
  def init(opts) do
    with {:ok, opts} <- Spark.OptionsHelpers.validate(opts, @opt_schema),
         {validation, validation_opts} = opts[:validation],
         true <- function_exported?(validation, :describe, 1),
         {:module, validation} <- Code.ensure_compiled(validation),
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

        {:error, "#{module} must implement `describe/1` function to be used in #{__MODULE__}"}

      error ->
        error
    end
  end

  @impl true
  def validate(changeset, opts) do
    {validation, validation_opts} = opts[:validation]

    case validation.validate(changeset, validation_opts) do
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
  def describe(opts) do
    {validation, validation_opts} = opts[:validation]
    [message: message, vars: vars] = validation.describe(validation_opts)

    [
      message: "must not pass validation: \"#{message}\"",
      vars: vars
    ]
  end
end
