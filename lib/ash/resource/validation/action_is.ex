defmodule Ash.Resource.Validation.ActionIs do
  use Ash.Resource.Validation

  @impl true
  def validate(changeset, opts) do
    if changeset.action.name == opts[:action] do
      :ok
    else
      # We use "unknown" here because it doesn't make sense to surface
      # this error to clients potentially (and this should really only be used as a condition anyway)
      {:error, Ash.Error.Unknown.UnknownError.exception(error: "action must be #{opts[:action]}")}
    end
  end
end
