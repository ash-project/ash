defmodule Ash.Error.Invalid do
  @moduledoc "The top level invalid error"
  use Ash.Error.Exception

  use Splode.Error, fields: [:errors, :changeset, :query, :action_input], class: :invalid

  @type t :: %__MODULE__{}

  def message(%{errors: errors}) do
    Splode.ErrorClass.error_messages(errors)
  end
end
