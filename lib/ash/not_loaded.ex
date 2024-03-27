defmodule Ash.NotLoaded do
  @moduledoc "Used when a field hasn't been loaded or selected."
  defstruct [:field, :type]

  @type t :: %__MODULE__{
          field: atom,
          type: :relationship | :calculation | :aggregate | :attribute
        }

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(not_loaded, opts) do
      concat([
        "#Ash.NotLoaded<",
        to_doc(not_loaded.type, opts),
        ", field: ",
        to_doc(not_loaded.field, opts),
        ">"
      ])
    end
  end
end
