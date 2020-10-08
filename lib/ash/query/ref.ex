defmodule Ash.Query.Ref do
  @moduledoc "Represents a relation/attribute reference"
  defstruct [:attribute, :relationship_path, :resource]

  defimpl Inspect do
    def inspect(ref, _opts) do
      name =
        case ref.attribute do
          %{name: name} -> name
          name -> name
        end

      case ref.relationship_path do
        [] -> "#{name}"
        path -> Enum.join(path, ".") <> "." <> "#{name}"
      end
    end
  end
end
