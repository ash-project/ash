defmodule Ash.Error.Framework do
  @moduledoc "Used when an unknown/generic framework error occurs"
  use Splode.ErrorClass, fields: [:changeset, :query, :action_input], class: :framework

  @type t :: %__MODULE__{}
end
