defmodule Ash.Filter.Ref do
  @moduledoc "Represents a relation/attribute reference"
  defstruct [:attribute, :relationship_path, :resource]

  defimpl Inspect do
    def inspect(ref, _opts) do
      case ref.relationship_path do
        [] -> "#{ref.attribute.name}"
        path -> Enum.join(path, ".") <> "." <> "#{ref.attribute.name}"
      end
    end
  end
end
