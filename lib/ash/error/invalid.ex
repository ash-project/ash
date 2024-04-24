defmodule Ash.Error.Invalid do
  @moduledoc "The top level invalid error"
  use Splode.ErrorClass, fields: [:changeset, :query, :action_input], class: :invalid

  @type t :: %__MODULE__{}
end
