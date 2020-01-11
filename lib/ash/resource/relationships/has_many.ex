defmodule Ash.Resource.Relationships.HasMany do
  defstruct [
    :name,
    :type,
    :cardinality,
    :destination,
    :destination_field,
    :source_field,
    :write_rules,
    :source,
    :reverse_relationship,
    :write_rules
  ]

  @type t :: %__MODULE__{
          type: :has_many,
          cardinality: :many,
          source: Ash.resource(),
          write_rules: Keyword.t(),
          name: atom,
          type: Ash.Type.t(),
          destination: Ash.resource(),
          destination_field: atom,
          source_field: atom,
          reverse_relationship: atom() | nil
        }

  @opt_schema Ashton.schema(
                opts: [
                  destination_field: :atom,
                  source_field: :atom,
                  write_rules: :keyword,
                  reverse_relationship: :atom
                ],
                defaults: [
                  source_field: :id,
                  write_rules: []
                ],
                describe: [
                  reverse_relationship:
                    "A requirement for side loading data. Must be the name of an inverse relationship on the destination resource.",
                  destination_field:
                    "The field on the related resource that should match the `source_field` on this resource. Default: [resource.name]_id",
                  source_field:
                    "The field on this resource that should match the `destination_field` on the related resource.",
                  write_rules: """
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
          name :: atom,
          related_resource :: Ash.resource(),
          opts :: Keyword.t()
        ) :: {:ok, t()} | {:error, term}
  def new(resource, resource_type, name, related_resource, opts \\ []) do
    # Don't call functions on the resource! We don't want it to compile here
    case Ashton.validate(opts, @opt_schema) do
      {:ok, opts} ->
        write_rules =
          case opts[:write_rules] do
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
           write_rules: write_rules,
           source: resource,
           type: :has_many,
           cardinality: :many,
           destination: related_resource,
           destination_field: opts[:destination_field] || :"#{resource_type}_id",
           source_field: opts[:source_field],
           reverse_relationship: opts[:reverse_relationship]
         }}

      {:error, error} ->
        {:error, error}
    end
  end
end
