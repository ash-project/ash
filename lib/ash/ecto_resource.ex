defmodule Ash.EctoResource do
  # TODO: write good module doc
  @moduledoc """

  explain how people can use this helper script
  ```
  schemas = [Foo, Bar, Baz]
  repo = MyApp.Repo

  for schema <- schemas do
    schema
    |> Module.concat(Resource)
    |> Module.create(
      quote do
        defmodule Module.concat(unquote(schema), Resource) do
          use Ash.EctoResource, schema: unquote(schema), data_layer: AshPostgres.DataLayer

          postgres do
            repo unquote(repo)
            table unquote(schema).__schema__(:source)
          end
        end
    end, Macro.Env.location(__ENV__))
  end
  ```
  """
  defmacro __using__(opts) do
    quote do
      use Ash.Resource, unquote(Keyword.delete(opts, :schema))

      @source_schema unquote(opts[:schema])
      @resource_module unquote(opts[:resource_module]) || AshResource

      attributes do
        primary_key = @source_schema.__schema__(:primary_key)

        association_ids =
          Enum.map(@source_schema.__schema__(:associations), fn name -> :"#{name}_id" end)

        for field <- @source_schema.__schema__(:fields) -- association_ids do
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

      relationships do
        for assoc <- @source_schema.__schema__(:associations) do
          case @source_schema.__schema__(:association, assoc) do
            %Ecto.Association.BelongsTo{field: field, related: related} ->
              belongs_to(field, Module.concat(related, @resource_module))

            %Ecto.Association.Has{
              cardinality: :one,
              field: field,
              related: related,
              related_key: related_key
            } ->
              has_one(field, Module.concat(related, @resource_module),
                destination_field: related_key
              )

            %Ecto.Association.Has{
              cardinality: :many,
              field: field,
              related: related,
              related_key: related_key
            } ->
              has_many(field, Module.concat(related, @resource_module),
                destination_field: related_key
              )

            %Ecto.Association.ManyToMany{} ->
              raise "many_to_many relationships not currently supported"
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
