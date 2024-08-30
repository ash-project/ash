defmodule Ash.Error.Forbidden.InitialDataRequired do
  @moduledoc "Used when initial data was not supplied when it was required"
  use Splode.Error, fields: [:source], class: :forbidden

  def message(%{source: source}) do
    "initial data is required for authorization for `#{source}`"
  end
end
