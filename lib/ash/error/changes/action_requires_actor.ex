defmodule Ash.Error.Changes.ActionRequiresActor do
  @moduledoc "Used when an actor is referenced in a filter template, but no actor exists"
  use Ash.Error.Exception

  use Splode.Error, fields: [], class: :invalid

  def message(_error) do
    "actor is required"
  end
end
