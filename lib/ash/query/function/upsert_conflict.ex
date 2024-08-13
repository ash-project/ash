defmodule Ash.Query.UpsertConflict do
  @moduledoc """
  Returns the conflicting new information.
  """

  defstruct [:attribute]

  def new(attribute) when is_atom(attribute) do
    %__MODULE__{attribute: attribute}
  end

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(%{attribute: attribute}, opts) do
      concat([
        "upsert_conflict(",
        to_doc(attribute, opts),
        ")"
      ])
    end
  end
end
