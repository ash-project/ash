defmodule Ash.Error.Forbidden.InitialDataRequired do
  @moduledoc "Used when "
  use Ash.Error.Exception

  use Splode.Error, fields: [:source], class: :forbidden

  def message(%{source: source}) do
    "initial data is required for authorization for `#{source}`"
  end
end
