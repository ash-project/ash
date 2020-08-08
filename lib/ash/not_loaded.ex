defmodule Ash.NotLoaded do
  @moduledoc "Used when an aggregate or relationship hasn't been loaded."
  defstruct [:field, :type]

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(not_loaded, opts) do
      concat(["#Ash.NotLoaded<", to_doc(not_loaded.type, opts), ">"])
    end
  end
end
