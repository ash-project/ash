defmodule Ash do
  @moduledoc """
  The primary interface for interrogating apis and resources.

  This is not the code level interface for a resource. Instead, call functions
  on an `Api` module that contains those resources.
  """
  alias Ash.Resource.Relationships.{BelongsTo, HasOne, HasMany, ManyToMany}
  alias Ash.Resource.Actions.{Create, Read, Update, Destroy}

  @type record :: struct
  @type cardinality_one_relationship() :: HasOne.t() | BelongsTo.t()
  @type cardinality_many_relationship() :: HasMany.t() | ManyToMany.t()
  @type relationship :: cardinality_one_relationship() | cardinality_many_relationship()
  @type query :: struct
  @type resource :: module
  @type data_layer :: module
  @type api :: module
  @type error :: struct
  @type filter :: map()
  @type sort :: Keyword.t()
  @type side_loads :: Keyword.t()
  @type attribute :: Ash.Attributes.Attribute.t()
  @type action :: Create.t() | Read.t() | Update.t() | Destroy.t()
  @type side_load_type :: :simple | :parallel
  @type side_load_config :: {side_load_type, Keyword.t()}

  @spec data_layer_can?(resource(), Ash.DataLayer.feature()) :: boolean
  def data_layer_can?(resource, feature) do
    data_layer = data_layer(resource)

    data_layer && data_layer.can?(feature)
  end

  @spec resources(api) :: list(resource())
  def resources(api) do
    api.resources()
  end

  @spec primary_key(resource()) :: list(attribute)
  def primary_key(resource) do
    resource.primary_key()
  end

  @spec relationship(resource(), atom() | String.t()) :: relationship() | nil
  def relationship(resource, relationship_name) when is_bitstring(relationship_name) do
    Enum.find(resource.relationships(), &(to_string(&1.name) == relationship_name))
  end

  def relationship(resource, relationship_name) do
    Enum.find(resource.relationships(), &(&1.name == relationship_name))
  end

  @spec relationships(resource()) :: list(relationship())
  def relationships(resource) do
    resource.relationships()
  end

  @spec side_load_config(api()) :: side_load_config()
  def side_load_config(api) do
    api.side_load_config()
  end

  @spec primary_action(resource(), atom()) :: action() | nil
  def primary_action(resource, type) do
    resource
    |> actions()
    |> Enum.filter(&(&1.type == type))
    |> case do
      [action] -> action
      actions -> Enum.find(actions, & &1.primary?)
    end
  end

  @spec action(resource(), atom(), atom()) :: action() | nil
  def action(resource, name, type) do
    Enum.find(resource.actions(), &(&1.name == name && &1.type == type))
  end

  @spec actions(resource()) :: list(action())
  def actions(resource) do
    resource.actions()
  end

  @spec attribute(resource(), String.t() | atom) :: attribute() | nil
  def attribute(resource, name) when is_bitstring(name) do
    Enum.find(resource.attributes, &(to_string(&1.name) == name))
  end

  def attribute(resource, name) do
    Enum.find(resource.attributes, &(&1.name == name))
  end

  @spec attributes(resource()) :: list(attribute())
  def attributes(resource) do
    resource.attributes()
  end

  @spec name(resource()) :: String.t()
  def name(resource) do
    resource.name()
  end

  @spec type(resource()) :: String.t()
  def type(resource) do
    resource.type()
  end

  @spec max_page_size(api(), resource()) :: non_neg_integer() | nil
  def max_page_size(api, resource) do
    min(api.max_page_size(), resource.max_page_size())
  end

  @spec default_page_size(api(), resource()) :: non_neg_integer() | nil
  def default_page_size(api, resource) do
    min(api.default_page_size(), resource.default_page_size())
  end

  @spec data_layer(resource()) :: data_layer()
  def data_layer(resource) do
    resource.data_layer()
  end
end
