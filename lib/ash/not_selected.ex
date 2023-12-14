defmodule Ash.NotSelected do
  @moduledoc "Used when an aggregate or relationship hasn't been loaded."
  defstruct [:field]

  @type t :: %__MODULE__{
          field: atom
        }

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(not_loaded, opts) do
      concat(["#Ash.NotSelected<", to_doc(not_loaded.field, opts), ">"])
    end
  end
end
