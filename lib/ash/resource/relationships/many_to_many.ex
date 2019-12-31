defmodule Ash.Resource.Relationships.ManyToMany do
  defstruct [
    :name,
    :type,
    :source,
    :through,
    :cardinality,
    :destination,
    :source_field,
    :destination_field,
    :source_field_on_join_table,
    :destination_field_on_join_table,
    :reverse_relationship,
    :authorization_steps
  ]

  @type t :: %__MODULE__{
          type: :many_to_many,
          cardinality: :many,
          source: Ash.resource(),
          name: atom,
          through: Ash.resource(),
          destination: Ash.resource(),
          source_field: atom,
          destination_field: atom,
          source_field_on_join_table: atom,
          destination_field_on_join_table: atom,
          reverse_relationship: atom,
          authorization_steps: Keyword.t()
        }

  @opt_schema Ashton.schema(
                opts: [
                  source_field_on_join_table: :atom,
                  destination_field_on_join_table: :atom,
                  source_field: :atom,
                  destination_field: :atom,
                  authorization_steps: :keyword,
                  through: :atom,
                  reverse_relationship: :atom,
                  authorization_steps: :keyword
                ],
                defaults: [
                  source_field: :id,
                  destination_field: :id,
                  authorization_steps: []
                ],
                required: [
                  :through
                ],
                describe: [
                  through: "The resource to use as the join table.",
                  reverse_relationship:
                    "A requirement for side loading data. Must be the name of an inverse relationship on the destination resource.",
                  source_field_on_join_table:
                    "The field on the join table that should line up with `source_field` on this resource. Default: [resource_name]_id",
                  destination_field_on_join_table:
                    "The field on the join table that should line up with `destination_field` on the related resource. Default: [relationshihp_name]_id",
                  source_field:
                    "The field on this resource that should line up with `source_field_on_join_table` on the join table.",
                  destination_field:
                    "The field on the related resource that should line up with `destination_field_on_join_table` on the join table.",
                  authorization_steps: """
                  Steps applied on an relationship during create or update. If no steps are defined, authorization to change will fail.
                  If set to false, no steps are applied and any changes are allowed (assuming the action was authorized as a whole)
                  Remember that any changes against the destination records *will* still be authorized regardless of this setting.
                  """
                ]
              )

  @doc false
  def opt_schema(), do: @opt_schema

  @spec new(
          resource :: Ash.resource(),
          resource_name :: String.t(),
          name :: atom,
          related_resource :: Ash.resource(),
          opts :: Keyword.t()
        ) :: {:ok, t()} | {:error, term}
  def new(resource, resource_name, name, related_resource, opts \\ []) do
    # Don't call functions on the resource! We don't want it to compile here
    case Ashton.validate(opts, @opt_schema) do
      {:ok, opts} ->
        authorization_steps =
          case opts[:authorization_steps] do
            false ->
              false

            steps ->
              base_attribute_opts = [
                relationship_name: name,
                destination: related_resource,
                resource: resource
              ]

              Enum.map(steps, fn {step, {mod, opts}} ->
                {step, {mod, Keyword.merge(base_attribute_opts, opts)}}
              end)
          end

        {:ok,
         %__MODULE__{
           name: name,
           type: :many_to_many,
           source: resource,
           cardinality: :many,
           through: opts[:through],
           destination: related_resource,
           reverse_relationship: opts[:reverse_relationship],
           source_field: opts[:source_field],
           destination_field: opts[:destination_field],
           authorization_steps: authorization_steps,
           source_field_on_join_table:
             opts[:source_field_on_join_table] || :"#{resource_name}_id",
           destination_field_on_join_table:
             opts[:destination_field_on_join_table] || :"#{name}_id"
         }}

      {:error, errors} ->
        {:error, errors}
    end
  end
end
