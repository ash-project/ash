defmodule Ash.Error.Query.NoComplexSortsWithKeysetPagination do
  @moduledoc """
  Due to the filter-based implementation of keyset pagination, it cannot be used with sorts on calculations.

  We could solve this problem by making the keyset only be the primary key of the record,
  and then fetching that value loading the calculations/aggregates that we need. If we do this
  we should either: 1.) make it a new pagination mode or 2.) add an option like `mode: :strict | :fetch`
  to pagination options.

  Let me know if you're reading this and want to help implement it.
  """
  use Ash.Error.Exception

  def_ash_error([:resource, :sort], class: :framework)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "no_complex_sorts_with_keyset_pagination"

    def class(_), do: :invalid

    def message(%{resource: resource, sort: sort}) do
      """
      Attempted to sort by a calculation or aggregate while using keyset pagination with #{inspect(resource)}

      This is not currently supported.

      Sort: #{inspect(sort)}
      """
    end

    def stacktrace(_), do: nil
  end
end
