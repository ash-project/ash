defmodule Ash.Error.Query.NoSuchFilterPredicate do
  @moduledoc "Used when a filter predicate that does not exist is referenced"
  use Ash.Error.Exception

  use Splode.Error, fields: [:message, :value, :key, :resource], class: :invalid

  def message(%{key: key, resource: resource}) do
    "No such filter predicate for #{inspect(resource)}: #{inspect(key)}"
  end
end
