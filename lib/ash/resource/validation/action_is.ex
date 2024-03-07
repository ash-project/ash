defmodule Ash.Resource.Validation.ActionIs do
  @moduledoc "Validates that the action is the specified action."
  use Ash.Resource.Validation

  @impl true
  def validate(changeset, opts, _context) do
    if changeset.action.name == opts[:action] do
      :ok
    else
      # We use "unknown" here because it doesn't make sense to surface
      # this error to clients potentially (and this should really only be used as a condition anyway)
      [message: message, vars: vars] = describe(opts)

      {:error, Ash.Error.Unknown.UnknownError.exception(error: message, vars: vars)}
    end
  end

  @impl true
  def atomic(changeset, opts, context) do
    validate(changeset, opts, context)
  end

  @impl true
  def describe(opts) do
    [message: "must be %{action}", vars: %{action: opts[:action]}]
  end
end
