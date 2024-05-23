defmodule Ash.Error.Query.CalculationRequiresPrimaryKey do
  @moduledoc "Used when a calculation requires a primary key but was not supplied with one"
  use Ash.Error.Exception

  use Splode.Error, fields: [:resource, :calculation], class: :invalid

  def message(error) do
    identifier =
      if String.Chars.impl_for(error.calculation) do
        "#{inspect(error.resource)}.#{error.calculation}"
      else
        "#{inspect(error.resource)}.#{inspect(error.calculation)}"
      end

    """
    Primary key is required for #{identifier}, as it uses aggregates or `exists` expressions.

    In practice, this means accepting a record, or adding arguments for each key in the primary key.
    """
  end
end
