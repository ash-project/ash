defmodule Ash.EctoResource do
  defmacro __using__(opts) do
    quote do
      use Ash.Resource, unquote(Keyword.delete(opts, :schema))

      @source_schema unquote(opts[:schema])

      attributes do
        primary_key = @source_schema.__schema__(:primary_key)

        for field <- @source_schema.__schema__(:fields) do
          type = @source_schema.__schema__(:type, field)
          pkey? = field in primary_key

          case {type, pkey?} do
            {uuid, true} when uuid in [Ecto.UUID, :binary_id] ->
              uuid_primary_key field

            {:id, true} ->
              integer_primary_key field

            {type, pkey?} ->
              attribute field, type, primary_key?: pkey?
          end
        end
      end

      # we only want to enable reads by default, for other actions you should migrate a resource to Ash.Resource
      actions do
        defaults [:read]
      end
    end
  end
end
