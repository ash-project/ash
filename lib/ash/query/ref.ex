defmodule Ash.Query.Ref do
  @moduledoc "Represents a relation/attribute reference"
  defstruct [:attribute, :relationship_path, :resource, :simple_equality?, :bare?]

  @doc "Returns the referenced field"
  def name(%__MODULE__{attribute: %{name: name}}), do: name
  def name(%__MODULE__{attribute: name}), do: name

  defimpl Inspect do
    def inspect(ref, _opts) do
      case ref.attribute do
        %Ash.Query.Calculation{} ->
          inspect(%Ash.Query.Call{
            name: ref.attribute.name,
            relationship_path: ref.relationship_path,
            args: [Map.delete(ref.attribute.context, :context)]
          })

        _ ->
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
end
