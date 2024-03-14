defmodule Ash.Error.Framework do
  @moduledoc "Used when an unknown/generic framework error occurs"
  use Ash.Error.Exception

  use Splode.Error, fields: [:errors, :changeset, :query, :action_input], class: :framework

  @type t :: %__MODULE__{}

  def splode_message(%{errors: errors}) do
    Splode.ErrorClass.error_messages(errors)
  end
end
