# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Info.Manifest.Generator.TypeResolver do
  @moduledoc """
  Resolves Ash types into `%Ash.Info.Manifest.Type{}` structs.

  Handles all Ash type variants: primitives, NewTypes, arrays, enums, unions,
  embedded resources, structs, maps, keywords, tuples, and custom types.
  """

  alias Ash.Info.Manifest.Type

  # Primitive type mapping: Ash module → {kind, name}
  @primitives %{
    Ash.Type.String => {:string, "String"},
    Ash.Type.CiString => {:ci_string, "CiString"},
    Ash.Type.Integer => {:integer, "Integer"},
    Ash.Type.Float => {:float, "Float"},
    Ash.Type.Decimal => {:decimal, "Decimal"},
    Ash.Type.Boolean => {:boolean, "Boolean"},
    Ash.Type.UUID => {:uuid, "UUID"},
    Ash.Type.UUIDv7 => {:uuid, "UUIDv7"},
    Ash.Type.Date => {:date, "Date"},
    Ash.Type.Time => {:time, "Time"},
    Ash.Type.TimeUsec => {:time_usec, "TimeUsec"},
    Ash.Type.DateTime => {:datetime, "DateTime"},
    Ash.Type.UtcDatetime => {:utc_datetime, "UtcDateTime"},
    Ash.Type.UtcDatetimeUsec => {:utc_datetime_usec, "UtcDateTimeUsec"},
    Ash.Type.NaiveDatetime => {:naive_datetime, "NaiveDateTime"},
    Ash.Type.Duration => {:duration, "Duration"},
    Ash.Type.Binary => {:binary, "Binary"},
    Ash.Type.UrlEncodedBinary => {:binary, "UrlEncodedBinary"},
    Ash.Type.Term => {:term, "Term"},
    Ash.Type.Atom => {:atom, "Atom"},
    Ash.Type.Module => {:atom, "Module"},
    Ash.Type.File => {:term, "File"},
    Ash.Type.Function => {:term, "Function"},
    Ash.Type.Vector => {:term, "Vector"}
  }

  @doc """
  Resolve an Ash type and its constraints into an `%Ash.Info.Manifest.Type{}`.

  Named type modules (Ash.Type.Enum implementations and Ash.Type.NewType subtypes)
  are resolved as `kind: :type_ref` references. Use `resolve_definition/1` to get
  the full type definition for `spec.types`.
  """
  @spec resolve(term(), term()) :: Type.t()
  def resolve(type, constraints \\ [])

  def resolve(nil, _constraints) do
    %Type{kind: :unknown, name: "Unknown", module: nil, constraints: []}
  end

  def resolve({:array, inner_type}, constraints) do
    items_constraints = Keyword.get(constraints, :items, [])

    %Type{
      kind: :array,
      name: "Array",
      module: nil,
      constraints: constraints,
      item_type: resolve(inner_type, items_constraints)
    }
  end

  def resolve(type, constraints) when is_atom(type) do
    # Canonicalize atom shorthands (e.g. `:string`) to their module form
    # (`Ash.Type.String`). For modules and unrecognized atoms, get_type/1
    # returns the input unchanged.
    type = Ash.Type.get_type(type)

    case Map.get(@primitives, type) do
      {:atom, name} ->
        atom_or_enum(type, name, constraints)

      {kind, name} ->
        %Type{kind: kind, name: name, module: type, constraints: constraints}

      nil ->
        # Named type modules (enums and NewTypes) become references.
        # Their full definitions live in spec.types.
        if named_type_module?(type) do
          %Type{
            kind: :type_ref,
            name: type_name_from_module(type),
            module: type,
            constraints: constraints
          }
        else
          resolve_complex(type, constraints)
        end
    end
  end

  def resolve(_type, constraints) do
    %Type{kind: :unknown, name: "Unknown", module: nil, constraints: constraints}
  end

  @doc """
  Resolve a named type module into its full definition for `spec.types`.

  Unlike `resolve/2`, which emits `kind: :type_ref` for named type modules,
  this function expands the top-level type. Nested named type references
  within the definition still become `:type_ref`.
  """
  @spec resolve_definition(atom()) :: Type.t()
  def resolve_definition(type_module) when is_atom(type_module) do
    cond do
      is_enum_type?(type_module) ->
        resolve_enum(type_module, [])

      Ash.Type.NewType.new_type?(type_module) ->
        {unwrapped, constraints} = unwrap_new_type(type_module, [])
        resolved = resolve(unwrapped, constraints)
        %{resolved | module: type_module, name: type_name_from_module(type_module)}

      true ->
        resolve(type_module, [])
    end
  end

  @doc """
  Returns true if the given type is a named type module (Ash.Type.Enum or Ash.Type.NewType).
  """
  @spec named_type_module?(term()) :: boolean()
  def named_type_module?(type) when is_atom(type) do
    Code.ensure_loaded?(type) == true and
      (is_enum_type?(type) or Ash.Type.NewType.new_type?(type))
  end

  def named_type_module?(_), do: false

  # Builds either an `:atom` or an `:enum` Type depending on whether
  # `:one_of` constraints are present. Shared between the atom shorthand
  # path and the module path.
  defp atom_or_enum(module_or_atom, name, constraints) do
    case Keyword.get(constraints, :one_of) do
      values when is_list(values) and values != [] ->
        %Type{
          kind: :enum,
          name: "Enum",
          module: module_or_atom,
          constraints: constraints,
          values: values
        }

      _ ->
        %Type{kind: :atom, name: name, module: module_or_atom, constraints: constraints}
    end
  end

  defp resolve_complex(type, constraints) when is_atom(type) do
    cond do
      # Enums
      is_enum_type?(type) ->
        resolve_enum(type, constraints)

      # Unions
      type == Ash.Type.Union ->
        resolve_union(constraints)

      # Embedded resources (direct module reference)
      is_embedded_resource?(type) ->
        %Type{
          kind: :embedded_resource,
          name: resource_name(type),
          module: type,
          constraints: constraints,
          resource_module: type
        }

      # Struct with instance_of pointing to a resource
      type == Ash.Type.Struct ->
        resolve_struct(constraints)

      # Map with fields
      type == Ash.Type.Map ->
        resolve_container(:map, "Map", constraints)

      # Keyword with fields
      type == Ash.Type.Keyword ->
        resolve_container(:keyword, "Keyword", constraints)

      # Tuple with fields
      type == Ash.Type.Tuple ->
        resolve_tuple(constraints)

      # Non-embedded resource
      is_resource?(type) ->
        %Type{
          kind: :resource,
          name: resource_name(type),
          module: type,
          constraints: constraints,
          resource_module: type
        }

      # Fallback: unknown
      true ->
        %Type{
          kind: :unknown,
          name: type_name_from_module(type),
          module: type,
          constraints: constraints
        }
    end
  end

  defp resolve_enum(type, constraints) do
    values =
      if Code.ensure_loaded?(type) and function_exported?(type, :values, 0) do
        type.values()
      else
        []
      end

    %Type{
      kind: :enum,
      name: type_name_from_module(type),
      module: type,
      constraints: constraints,
      values: values
    }
  end

  defp resolve_union(constraints) do
    union_types = Keyword.get(constraints, :types, [])

    members =
      Enum.map(union_types, fn {name, config} ->
        member_type = Keyword.get(config, :type)
        member_constraints = Keyword.get(config, :constraints, [])

        member = %{
          name: name,
          type: resolve(member_type, member_constraints),
          description: Keyword.get(config, :description)
        }

        # Include tag info when present (for tagged unions)
        member =
          case Keyword.get(config, :tag) do
            nil -> member
            tag -> Map.put(member, :tag, tag)
          end

        case Keyword.get(config, :tag_value) do
          nil -> member
          tag_value -> Map.put(member, :tag_value, tag_value)
        end
      end)

    %Type{
      kind: :union,
      name: "Union",
      module: Ash.Type.Union,
      constraints: constraints,
      members: members
    }
  end

  defp resolve_struct(constraints) do
    instance_of = Keyword.get(constraints, :instance_of)
    fields = Keyword.get(constraints, :fields)

    cond do
      instance_of && is_resource?(instance_of) ->
        kind = if is_embedded_resource?(instance_of), do: :embedded_resource, else: :resource

        %Type{
          kind: kind,
          name: resource_name(instance_of),
          module: Ash.Type.Struct,
          constraints: constraints,
          resource_module: instance_of,
          instance_of: instance_of
        }

      instance_of && fields ->
        %Type{
          kind: :struct,
          name: type_name_from_module(instance_of),
          module: Ash.Type.Struct,
          constraints: constraints,
          instance_of: instance_of,
          fields: resolve_field_constraints(fields)
        }

      fields ->
        %Type{
          kind: :struct,
          name: "Struct",
          module: Ash.Type.Struct,
          constraints: constraints,
          fields: resolve_field_constraints(fields)
        }

      instance_of ->
        %Type{
          kind: :struct,
          name: type_name_from_module(instance_of),
          module: Ash.Type.Struct,
          constraints: constraints,
          instance_of: instance_of
        }

      true ->
        %Type{kind: :struct, name: "Struct", module: Ash.Type.Struct, constraints: constraints}
    end
  end

  defp resolve_container(kind, name, constraints) do
    fields = Keyword.get(constraints, :fields)

    if fields do
      %Type{
        kind: kind,
        name: name,
        module: container_module(kind),
        constraints: constraints,
        fields: resolve_field_constraints(fields)
      }
    else
      %Type{kind: kind, name: name, module: container_module(kind), constraints: constraints}
    end
  end

  defp resolve_tuple(constraints) do
    fields = Keyword.get(constraints, :fields)

    if fields do
      %Type{
        kind: :tuple,
        name: "Tuple",
        module: Ash.Type.Tuple,
        constraints: constraints,
        element_types: resolve_field_constraints(fields)
      }
    else
      %Type{kind: :tuple, name: "Tuple", module: Ash.Type.Tuple, constraints: constraints}
    end
  end

  defp resolve_field_constraints(fields) when is_list(fields) do
    Enum.map(fields, fn {name, config} ->
      field_type = Keyword.get(config, :type)
      field_constraints = Keyword.get(config, :constraints, [])
      allow_nil? = Keyword.get(config, :allow_nil?, true)

      %{
        name: name,
        type: resolve(field_type, field_constraints),
        allow_nil?: allow_nil?,
        description: Keyword.get(config, :description)
      }
    end)
  end

  defp resolve_field_constraints(_), do: []

  @doc false
  def unwrap_new_type(type, constraints) when is_atom(type) do
    if Code.ensure_loaded?(type) == true and
         Ash.Type.NewType.new_type?(type) do
      subtype = Ash.Type.NewType.subtype_of(type)

      constraints =
        case type.do_init(constraints) do
          {:ok, merged_constraints} -> merged_constraints
          {:error, _} -> constraints
        end

      {subtype, constraints}
    else
      {type, constraints}
    end
  end

  def unwrap_new_type(type, constraints), do: {type, constraints}

  defp is_embedded_resource?(module) when is_atom(module) do
    is_resource?(module) and Ash.Resource.Info.embedded?(module)
  end

  defp is_resource?(module) when is_atom(module) do
    Code.ensure_loaded?(module) == true and Ash.Resource.Info.resource?(module)
  end

  defp is_resource?(_), do: false

  defp is_enum_type?(type) when is_atom(type) do
    Code.ensure_loaded?(type) == true and
      Spark.implements_behaviour?(type, Ash.Type.Enum)
  end

  defp resource_name(module) do
    module
    |> Module.split()
    |> List.last()
  end

  defp type_name_from_module(module) when is_atom(module) do
    if Code.ensure_loaded?(module) == true do
      module
      |> Module.split()
      |> List.last()
    else
      to_string(module)
    end
  end

  defp type_name_from_module(other), do: inspect(other)

  defp container_module(:map), do: Ash.Type.Map
  defp container_module(:keyword), do: Ash.Type.Keyword
end
