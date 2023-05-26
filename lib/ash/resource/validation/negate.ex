defmodule Ash.Resource.Validation.Negate do
  @moduledoc false

  alias Ash.Error.Changes.InvalidAttribute

  @opt_schema [
    validation: [
      type: Ash.Resource.Validation.validation_type(),
      required: true,
      doc: "The validation module to negate another validation"
    ]
  ]

  use Ash.Resource.Validation

  @impl true
  def init(opts) do
    with {:ok, opts} <- Spark.OptionsHelpers.validate(opts, @opt_schema),
         {validation, validation_opts} = opts[:validation],
         {:ok, _opts} <- validation.init(validation_opts) do
      {:ok, opts}
    else
      {:error, exception} = error when is_binary(exception) ->
        error

      {:error, error} ->
        {:error, Exception.message(error)}
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
         InvalidAttribute.exception(
           message: "must not pass validation %{validation}",
           vars: [validation: opts[:validation]]
         )}
    end
  end
end
