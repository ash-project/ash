defmodule Ash.Resource.Schema do
  defmacro define_schema(name) do
    quote do
      use Ecto.Schema

      schema unquote(name) do
        for {field_name, config} <- @attributes do
          unless field_name == :id do
            field field_name, config[:ecto_type]
          end
        end
      end
    end
  end
end
