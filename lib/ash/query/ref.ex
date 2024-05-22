defmodule Ash.Query.Ref do
  @moduledoc "Represents a relation/attribute reference"
  defstruct [:attribute, :relationship_path, :resource, :simple_equality?, :bare?, :input?]

  @doc "Returns the referenced field"
  def name(%__MODULE__{attribute: %{name: name}}), do: name
  def name(%__MODULE__{attribute: name}), do: name

  defimpl Inspect do
    def inspect(ref, _opts) do
      name =
        case ref.attribute do
          %{name: name} -> name
          name -> name
        end

      name =
        if is_binary(name) or is_atom(name) do
          to_string(name)
        else
          inspect(name)
        end

      case ref.relationship_path do
        [] ->
          name

        path ->
          Enum.map_join(path, ".", fn item ->
            case item do
              %struct{} when struct in [Ash.Query.Aggregate, Ash.Query.Calculation] ->
                inspect(item)

              %{name: name} ->
                name

              name ->
                name
            end
          end) <> "." <> "#{name}"
      end
    end
  end
end
