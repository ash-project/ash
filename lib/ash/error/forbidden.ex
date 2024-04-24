defmodule Ash.Error.Forbidden do
  @moduledoc "Used when authorization for an action fails"
  use Splode.ErrorClass, fields: [:changeset, :query, :action_input], class: :forbidden

  @type t :: %__MODULE__{}
end
