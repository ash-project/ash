# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Info.Manifest.Type do
  @moduledoc """
  Represents a resolved type in the API specification.

  Named type modules (Ash.Type.Enum implementations, Ash.Type.NewType subtypes,
  and embedded resources) are referenced inline with their full definitions
  living in `%Ash.Info.Manifest{}.types`. This prevents circular references and
  mirrors how non-embedded resources are referenced via `kind: :resource` with
  definitions in `%Ash.Info.Manifest{}.resources`.

  Embedded resources are *types*, not resources — they have no domain and no
  callable surface. The standalone entry in `manifest.types` has
  `kind: :embedded_resource` and `resource:` set to the full
  `%Ash.Info.Manifest.Resource{}` definition.

  Primitive types (string, integer, etc.) and anonymous containers (map/keyword/tuple
  without a named module) are still resolved inline.
  """

  @type kind ::
          :string
          | :integer
          | :boolean
          | :float
          | :decimal
          | :uuid
          | :date
          | :datetime
          | :utc_datetime
          | :utc_datetime_usec
          | :naive_datetime
          | :time
          | :time_usec
          | :duration
          | :binary
          | :atom
          | :ci_string
          | :term
          | :enum
          | :union
          | :resource
          | :embedded_resource
          | :map
          | :struct
          | :array
          | :tuple
          | :keyword
          | :type_ref
          | :any
          | :unknown

  @type t :: %__MODULE__{
          kind: kind(),
          name: String.t(),
          module: atom() | nil,
          constraints: keyword() | nil,
          allow_nil?: boolean() | nil,
          # For :enum
          values: [atom()] | nil,
          # For :union
          members: [%{name: atom(), type: t(), description: String.t() | nil}] | nil,
          # For :resource / :embedded_resource
          resource_module: atom() | nil,
          # For :map / :struct / :keyword
          fields:
            [%{name: atom(), type: t(), allow_nil?: boolean(), description: String.t() | nil}]
            | nil,
          # For :struct
          instance_of: atom() | nil,
          # For :array
          item_type: t() | nil,
          # For :tuple
          element_types:
            [%{name: atom(), type: t(), allow_nil?: boolean(), description: String.t() | nil}]
            | nil,
          # Set on standalone `kind: :embedded_resource` entries in `manifest.types`
          # to carry the full embedded resource definition. Inline references
          # leave this `nil` and resolve via `resource_module` lookup.
          resource: Ash.Info.Manifest.Resource.t() | nil,
          custom: map()
        }

  defstruct [
    :kind,
    :name,
    :module,
    :constraints,
    :allow_nil?,
    :values,
    :members,
    :resource_module,
    :fields,
    :instance_of,
    :item_type,
    :element_types,
    :resource,
    custom: %{}
  ]

  @doc """
  Returns the effective module for a type — `instance_of` if set, otherwise `module`.

  For struct types wrapping a module (NewTypes, TypedStructs), `instance_of` points
  to the original module. For other types, `module` is the Ash type module.
  """
  @spec effective_module(t()) :: atom() | nil
  def effective_module(%__MODULE__{instance_of: inst}) when not is_nil(inst), do: inst
  def effective_module(%__MODULE__{module: mod}), do: mod

  @doc """
  Returns the effective resource module for a type.

  For resource/embedded_resource types, returns `resource_module`.
  Falls back to `effective_module/1`.
  """
  @spec effective_resource(t()) :: atom() | nil
  def effective_resource(%__MODULE__{resource_module: rm}) when not is_nil(rm), do: rm
  def effective_resource(%__MODULE__{} = t), do: effective_module(t)

  @doc """
  Returns true if the type represents a resource (`:resource` or `:embedded_resource`).
  """
  @spec resource_kind?(t()) :: boolean()
  def resource_kind?(%__MODULE__{kind: kind}) when kind in [:resource, :embedded_resource],
    do: true

  def resource_kind?(_), do: false

  @doc """
  Returns true if the type has nested field descriptors (`.fields` or `.element_types`).
  """
  @spec has_fields?(t()) :: boolean()
  def has_fields?(%__MODULE__{fields: fields}) when is_list(fields) and fields != [], do: true

  def has_fields?(%__MODULE__{element_types: ets}) when is_list(ets) and ets != [], do: true

  def has_fields?(_), do: false

  @doc """
  Returns the list of field descriptors for a type.

  Checks `.fields` first (for map/struct/keyword), then `.element_types` (for tuple).
  Returns an empty list if neither is populated.

  Each field is a map with `:name`, `:type` (`%Ash.Info.Manifest.Type{}`), `:allow_nil?`,
  and `:description` (which may be nil).
  """
  @spec get_fields(t()) :: [
          %{name: atom(), type: t(), allow_nil?: boolean(), description: String.t() | nil}
        ]
  def get_fields(%__MODULE__{fields: fields}) when is_list(fields) and fields != [], do: fields

  def get_fields(%__MODULE__{element_types: ets}) when is_list(ets) and ets != [], do: ets

  def get_fields(_), do: []

  @doc """
  Finds a sub-field by name from the type's field descriptors.

  Returns the field map (`%{name, type, allow_nil?, description}`) or nil if not found.
  """
  @spec find_field(t(), atom()) ::
          %{name: atom(), type: t(), allow_nil?: boolean(), description: String.t() | nil}
          | nil
  def find_field(%__MODULE__{} = type, field_name) when is_atom(field_name) do
    type
    |> get_fields()
    |> Enum.find(fn f -> f.name == field_name end)
  end

  def find_field(_, _), do: nil

  @doc """
  Finds the type of a sub-field by name.

  Returns the `%Ash.Info.Manifest.Type{}` of the field, or nil if not found.
  """
  @spec find_field_type(t(), atom()) :: t() | nil
  def find_field_type(%__MODULE__{} = type, field_name) do
    case find_field(type, field_name) do
      %{type: field_type} -> field_type
      nil -> nil
    end
  end

  @doc """
  Finds a union member by name from the type's members.

  Returns the member map (`%{name, type, description, tag, tag_value}`) or nil.
  `:tag` and `:tag_value` are only present for tagged union members.
  """
  @spec find_member(t(), atom()) :: map() | nil
  def find_member(%__MODULE__{members: members}, member_name)
      when is_list(members) and is_atom(member_name) do
    Enum.find(members, fn m -> m.name == member_name end)
  end

  def find_member(_, _), do: nil
end
