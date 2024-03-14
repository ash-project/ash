defmodule Ash.Error.Forbidden do
  @moduledoc "Used when authorization for an action fails"

  use Ash.Error.Exception

  use Splode.Error, fields: [:errors, :changeset, :query, :action_input], class: :forbidden

  @type t :: %__MODULE__{}

  def splode_message(%{errors: errors}) do
    Splode.ErrorClass.error_messages(errors)
  end
end
