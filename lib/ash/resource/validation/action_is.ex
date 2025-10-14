# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Validation.ActionIs do
  @moduledoc "Validates that the action is the specified action."
  use Ash.Resource.Validation

  @opt_schema [
    action: [
      type: {:wrap_list, :atom},
      doc: "The action or actions to compare against",
      required: true
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
  def supports(_opts), do: [Ash.Changeset, Ash.Query, Ash.ActionInput]

  @impl true
  def validate(subject, opts, _context) do
    if subject.action.name in List.wrap(opts[:action]) do
      :ok
    else
      # We use "unknown" here because it doesn't make sense to surface
      # this error to clients potentially (and this should really only be used as a condition anyway)
      description = describe(opts)

      {:error,
       Ash.Error.Unknown.UnknownError.exception(
         error: description[:message],
         vars: description[:vars]
       )}
    end
  end

  @impl true
  def atomic(subject, opts, context) do
    validate(subject, opts, context)
  end

  @impl true
  def describe(opts) do
    case opts[:action] do
      actions when is_list(actions) ->
        [message: "action must be one of %{action}", vars: %{action: Enum.join(actions, ", ")}]

      _ ->
        [message: "must be %{action}", vars: %{action: opts[:action]}]
    end
  end
end
