defmodule Ash.Query.Ref do
  @moduledoc "Represents a relation/attribute reference"
  defstruct [:attribute, :relationship_path, :resource, :simple_equality?, :bare?, :input?]

  @doc "Returns the referenced field"
  def name(%__MODULE__{attribute: %{name: name}}), do: name
  def name(%__MODULE__{attribute: name}), do: name

  defimpl Inspect do
    def inspect(ref, _opts) do
      case ref.attribute do
        %Ash.Query.Calculation{} ->
          case Map.drop(ref.attribute.context || %{}, [:context, :ash]) do
            empty when empty == %{} ->
              inspect_ref(ref)

            args ->
              inspect(%Ash.Query.Call{
                name: ref.attribute.name,
                relationship_path: ref.relationship_path,
                args: [args]
              })
          end

        %Ash.Query.Aggregate{} ->
          case Map.drop(ref.attribute.context || %{}, [:context, :ash]) do
            empty when empty == %{} ->
              inspect_ref(ref)

            args ->
              inspect(%Ash.Query.Call{
                name: ref.attribute.name,
                relationship_path: ref.relationship_path,
                args: [args]
              })
          end

        _ ->
          inspect_ref(ref)
      end
    end

    defp inspect_ref(ref) do
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
          Enum.join(path, ".") <> "." <> "#{name}"
      end
    end
  end
end
