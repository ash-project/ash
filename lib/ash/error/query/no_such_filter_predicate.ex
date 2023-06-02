defmodule Ash.Error.Query.NoSuchFilterPredicate do
  @moduledoc "Used when a filter predicate that does not exist is referenced"
  use Ash.Error.Exception

  def_ash_error([:message, :value, :key, :resource], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "no_such_filter_predicate"

    def message(%{key: key, resource: resource}) do
      "No such filter predicate for #{inspect(resource)}: #{inspect(key)}"
    end
  end
end
