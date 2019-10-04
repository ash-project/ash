defmodule Ash.Resource.Schema do
  defmacro define_schema(name) do
    quote do
      use Ecto.Schema
      @primary_key {:id, :binary_id, autogenerate: true}
      @foreign_key_type :binary_id

      schema unquote(name) do
        for attribute <- @attributes do
          unless attribute.name == :id do
            field attribute.name, attribute.ecto_type
          end
        end

        for relationship <- Enum.filter(@relationships, &(&1.type == :belongs_to)) do
          belongs_to relationship.name, relationship.destination
        end
      end
    end
  end
end
