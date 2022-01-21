defmodule Ash.EctoResource do
  defmacro __using__(opts) do
    quote do
      use Ash.Resource
      @source_schema unquote(opts[:schema])

      attributes do
        Ash.EctoResource.primary_key_definition(@source_schema)

        for field <- @source_schema.__schema__(:fields) -- @source_schema.__schema__(:primary_key) do
          attribute field, @source_schema.__schema__(:type, field)
        end
      end
    end
  end

  defmacro primary_key_definition(schema) do
    quote do
      case unquote(schema).__schema__(:primary_key) do
        [field] ->
          case unquote(schema).__schema__(:type, field) do
            type when type in [Ecto.UUID, :binary_id] ->
              uuid_primary_key field

            :id ->
              integer_primary_key field

            type ->
              raise "#{type} not currently supported for primary keys"
          end

        _fields ->
          raise "Ash.EctoResource does not currently support multiple primary keys"
      end
    end
  end
end
