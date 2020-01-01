defmodule Ash.Resource.Relationships.HasOne do
  @doc false
  defstruct [
    :name,
    :type,
    :source,
    :cardinality,
    :destination,
    :destination_field,
    :source_field,
    :reverse_relationship,
    :authorization_steps,
    :allow_orphans?
  ]

  @type t :: %__MODULE__{
          type: :has_one,
          cardinality: :one,
          source: Ash.resource(),
          name: atom,
          type: Ash.Type.t(),
          authorization_steps: Keyword.t(),
          destination: Ash.resource(),
          destination_field: atom,
          source_field: atom,
          allow_orphans?: boolean,
          reverse_relationship: atom | nil
        }

  @opt_schema Ashton.schema(
                opts: [
                  destination_field: :atom,
                  source_field: :atom,
                  reverse_relationship: :atom,
                  authorization_steps: :keyword,
                  allow_orphans?: :boolean
                ],
                defaults: [
                  source_field: :id,
                  authorization_steps: [],
                  # TODO: When we add constraint expressions, we should validate this with that.
                  allow_orphans?: true
                ],
                describe: [
                  reverse_relationship:
                    "A requirement for side loading data. Must be the name of an inverse relationship on the destination resource.",
                  destination_field:
                    "The field on the related resource that should match the `source_field` on this resource. Default: [resource.name]_id",
                  source_field:
                    "The field on this resource that should match the `destination_field` on the related resource.",
                  # TODO: Explain this better
                  allow_orphans:
                    "Whether or not to allow orphaned records that would result in replaced relationships.",
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
          resource_type :: String.t(),
          name :: atom,
          related_resource :: Ash.resource(),
          opts :: Keyword.t()
        ) :: {:ok, t()} | {:error, term}
  @doc false
  def new(resource, resource_type, name, related_resource, opts \\ []) do
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
           source: resource,
           type: :has_one,
           cardinality: :one,
           destination: related_resource,
           destination_field: opts[:destination_field] || :"#{resource_type}_id",
           source_field: opts[:source_field],
           reverse_relationship: opts[:reverse_relationship],
           authorization_steps: authorization_steps
         }}

      {:error, errors} ->
        {:error, errors}
    end
  end
end
