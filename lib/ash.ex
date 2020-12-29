defmodule Ash do
  @moduledoc " Types and simple helpers for Ash"
  alias Ash.Resource.Actions.{Create, Destroy, Read, Update}
  alias Ash.Resource.Relationships.{BelongsTo, HasMany, HasOne, ManyToMany}

  @type action :: Create.t() | Read.t() | Update.t() | Destroy.t()
  @type action_type :: :read | :create | :update | :destroy
  @type actor :: Ash.record()
  @type aggregate :: Ash.Query.Aggregate.t() | Ash.Resource.Aggregate.t()
  @type aggregate_kind :: Ash.Query.Aggregate.kind()
  @type api :: module
  @type attribute :: Ash.Resource.Attribute.t()
  @type calculation :: Ash.Resource.Calculation.t()
  @type cardinality_many_relationship() :: HasMany.t() | ManyToMany.t()
  @type cardinality_one_relationship() :: HasOne.t() | BelongsTo.t()
  @type changeset :: Ash.Changeset.t()
  @type data_layer :: module
  @type data_layer_query :: struct
  @type error :: struct
  @type filter :: Ash.Filter.t()
  @type params :: Keyword.t()
  @type primary_key :: record() | map | term
  @type query :: Ash.Query.t()
  @type record :: struct
  @type relationship :: cardinality_one_relationship() | cardinality_many_relationship()
  @type relationship_cardinality :: :many | :one
  @type resource :: module
  @type side_loads :: term
  @type page :: Ash.Page.Keyset.t() | Ash.Page.Offset.t()
  @type sort_order ::
          :asc | :desc | :asc_nils_first | :asc_nils_last | :desc_nils_first | :desc_nils_last
  @type sort :: list(atom | {atom, sort_order})
  @type validation :: Ash.Resource.Validation.t()
  @type notification :: Ash.Notifier.Notification.t()

  require Ash.Dsl.Extension

  def implements_behaviour?(module, behaviour) do
    :attributes
    |> module.module_info()
    |> Enum.flat_map(fn
      {:behaviour, value} -> List.wrap(value)
      _ -> []
    end)
    |> Enum.any?(&(&1 == behaviour))
  end

  def uuid do
    Ecto.UUID.generate()
  end

  @doc "Returns all extensions of a resource or api"
  @spec extensions(resource() | api()) :: [module]
  def extensions(resource) do
    :persistent_term.get({resource, :extensions}, [])
  end
end
