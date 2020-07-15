defmodule Ash.Error.Filter.NoSuchFilterPredicate do
  @moduledoc "Used when a filter predicate that does not exist is referenced"
  use Ash.Error

  def_ash_error([:message, :value], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ecto.UUID.generate()

    def code(_), do: "no_such_filter_predicate"

    def class(_), do: :invalid

    def message(%{key: key, resource: resource}) do
      "No such filter predicate for #{inspect(resource)}: #{inspect(key)}"
    end

    def stacktrace(_), do: nil
  end
end
