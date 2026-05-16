# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Info.Manifest.JsonSerializer do
  @moduledoc """
  Serializes `%Ash.Info.Manifest{}` structs to JSON.

  Recursively converts structs to maps, converting module atoms to strings
  and omitting nil fields.
  """

  @doc """
  Serialize an `%Ash.Info.Manifest{}` to a JSON string.
  """
  @spec to_json(Ash.Info.Manifest.t(), keyword()) :: {:ok, String.t()} | {:error, term()}
  def to_json(%Ash.Info.Manifest{} = spec, opts \\ []) do
    pretty? = Keyword.get(opts, :pretty, false)

    map = to_map(spec)

    json_opts = if pretty?, do: [pretty: true], else: []

    {:ok, Jason.encode!(map, json_opts)}
  rescue
    e -> {:error, Exception.message(e)}
  end

  @doc """
  Convert an `%Ash.Info.Manifest{}` to a plain map (suitable for JSON encoding).
  """
  @spec to_map(Ash.Info.Manifest.t()) :: map()
  def to_map(%Ash.Info.Manifest{} = spec) do
    %{
      "schema_version" => Ash.Info.Manifest.schema_version(),
      "resources" => Enum.map(spec.resources, &serialize_resource/1),
      "types" => Enum.map(spec.types, &serialize_type/1),
      "entrypoints" => Enum.map(spec.entrypoints, &serialize_entrypoint/1)
    }
  end

  defp serialize_resource(%Ash.Info.Manifest.Resource{} = resource) do
    %{
      "name" => resource.name,
      "module" => module_to_string(resource.module),
      "embedded" => resource.embedded?,
      "primary_key" => Enum.map(resource.primary_key || [], &to_string/1),
      "fields" => serialize_named_map(resource.fields, &serialize_field/1),
      "relationships" => serialize_named_map(resource.relationships, &serialize_relationship/1)
    }
    |> put_if_present("description", resource.description)
    |> put_if_present("multitenancy", serialize_multitenancy(resource.multitenancy))
  end

  defp serialize_entrypoint(%Ash.Info.Manifest.Entrypoint{} = entrypoint) do
    # config is omitted from JSON — it carries internal extension-specific structs
    %{
      "resource" => module_to_string(entrypoint.resource),
      "action" => serialize_action(entrypoint.action)
    }
  end

  defp serialize_field(%Ash.Info.Manifest.Field{} = field) do
    base = %{
      "kind" => to_string(field.kind),
      "type" => serialize_type(field.type),
      "allow_nil" => field.allow_nil?,
      "writable" => field.writable?,
      "has_default" => field.has_default?,
      "filterable" => field.filterable?,
      "sortable" => field.sortable?,
      "primary_key" => field.primary_key?,
      "sensitive" => field.sensitive?,
      "select_by_default" => field.select_by_default?
    }

    base
    |> put_if_present("description", field.description)
    |> put_if_present("arguments", serialize_arguments_list(field.arguments))
    |> put_if_present("aggregate_kind", serialize_atom(field.aggregate_kind))
  end

  defp serialize_relationship(%Ash.Info.Manifest.Relationship{} = rel) do
    %{
      "type" => to_string(rel.type),
      "cardinality" => to_string(rel.cardinality),
      "destination" => module_to_string(rel.destination),
      "allow_nil" => rel.allow_nil?,
      "filterable" => rel.filterable?,
      "sortable" => rel.sortable?
    }
    |> put_if_present("description", rel.description)
  end

  defp serialize_action(%Ash.Info.Manifest.Action{} = action) do
    %{
      "type" => to_string(action.type),
      "primary" => action.primary?,
      "get" => action.get?,
      "arguments" => Enum.map(action.arguments || [], &serialize_argument/1),
      "metadata" => Enum.map(action.metadata || [], &serialize_metadata/1)
    }
    |> put_if_present("description", action.description)
    |> put_if_present("accept", serialize_atom_list(action.accept))
    |> put_if_present("require_attributes", serialize_atom_list(action.require_attributes))
    |> put_if_present("allow_nil_input", serialize_atom_list(action.allow_nil_input))
    |> put_if_present("returns", serialize_type(action.returns))
    |> put_if_present("pagination", serialize_pagination(action.pagination))
  end

  defp serialize_type(nil), do: nil

  defp serialize_type(%Ash.Info.Manifest.Type{} = type) do
    base = %{
      "kind" => to_string(type.kind),
      "name" => type.name,
      "allow_nil" => type.allow_nil?
    }

    base
    |> put_if_present("module", module_to_string(type.module))
    |> put_if_present("values", serialize_atom_list(type.values))
    |> put_if_present("members", serialize_members(type.members))
    |> put_if_present("resource_module", module_to_string(type.resource_module))
    |> put_if_present("fields", serialize_type_fields(type.fields))
    |> put_if_present("instance_of", module_to_string(type.instance_of))
    |> put_if_present("item_type", serialize_type(type.item_type))
    |> put_if_present("element_types", serialize_type_fields(type.element_types))
  end

  defp serialize_argument(%Ash.Info.Manifest.Argument{} = arg) do
    %{
      "name" => to_string(arg.name),
      "type" => serialize_type(arg.type),
      "allow_nil" => arg.allow_nil?,
      "has_default" => arg.has_default?,
      "sensitive" => arg.sensitive?
    }
    |> put_if_present("description", arg.description)
  end

  defp serialize_metadata(%Ash.Info.Manifest.Metadata{} = meta) do
    %{
      "name" => to_string(meta.name),
      "type" => serialize_type(meta.type),
      "allow_nil" => meta.allow_nil?
    }
    |> put_if_present("description", meta.description)
  end

  defp serialize_pagination(nil), do: nil

  defp serialize_pagination(%Ash.Info.Manifest.Pagination{} = page) do
    %{
      "offset" => page.offset?,
      "keyset" => page.keyset?,
      "required" => page.required?,
      "countable" => page.countable?,
      "default_limit" => page.default_limit,
      "max_page_size" => page.max_page_size
    }
  end

  defp serialize_multitenancy(nil), do: nil

  defp serialize_multitenancy(%{} = mt) do
    %{
      "strategy" => to_string(mt.strategy),
      "global" => mt.global?,
      "attribute" => serialize_atom(mt.attribute)
    }
  end

  defp serialize_named_map(map, serialize_fn) when is_map(map) do
    Map.new(map, fn {name, value} ->
      {to_string(name), serialize_fn.(value)}
    end)
  end

  defp serialize_arguments_list(nil), do: nil
  defp serialize_arguments_list([]), do: nil
  defp serialize_arguments_list(args), do: Enum.map(args, &serialize_argument/1)

  defp serialize_members(nil), do: nil

  defp serialize_members(members) do
    Enum.map(members, fn member ->
      %{
        "name" => to_string(member.name),
        "type" => serialize_type(member.type)
      }
    end)
  end

  defp serialize_type_fields(nil), do: nil

  defp serialize_type_fields(fields) do
    Enum.map(fields, fn field ->
      %{
        "name" => to_string(field.name),
        "type" => serialize_type(field.type),
        "allow_nil" => field.allow_nil?
      }
    end)
  end

  defp serialize_atom_list(nil), do: nil
  defp serialize_atom_list(list), do: Enum.map(list, &to_string/1)

  defp serialize_atom(nil), do: nil
  defp serialize_atom(atom) when is_atom(atom), do: to_string(atom)

  defp module_to_string(nil), do: nil

  defp module_to_string(module) when is_atom(module) do
    case Atom.to_string(module) do
      "Elixir." <> _ -> module |> Module.split() |> Enum.join(".")
      other -> other
    end
  end

  defp module_to_string(other), do: inspect(other)

  defp put_if_present(map, _key, nil), do: map
  defp put_if_present(map, key, value), do: Map.put(map, key, value)
end
